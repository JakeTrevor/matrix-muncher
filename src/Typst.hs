{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Typst (Typst (toTypst)) where

import Algebra
import Data.Complex
import Data.List (intercalate)
import Lib
import Matrix
import MatrixAlgebra

class Typst t where
  toTypst :: t -> String

escapeVar :: String -> String
escapeVar "*rt2" = " sqrt(2) "
escapeVar "rt2" = " sqrt(2) "
escapeVar ('*' : rest) = escapeVar rest ++ "^*"
escapeVar s
  | length s == 1 = s
  | otherwise = "\"" ++ s ++ "\""

instance Typst Double where
  toTypst 0 = "0"
  toTypst (0.5) = "1/2"
  toTypst (-0.5) = "-1/2"
  toTypst 1 = "1"
  toTypst (-1) = "-1"
  toTypst x = show x

instance Typst (Complex Double) where
  toTypst (a :+ 0) = toTypst a
  toTypst (0 :+ 1) = "i"
  toTypst (0 :+ -1) = "-i"
  toTypst (0.5 :+ 0.5) = "(1+i)/2"
  toTypst (0.5 :+ -0.5) = "(1-i)/2"
  toTypst (-0.5 :+ 0.5) = "(-1+i)/2"
  toTypst (-0.5 :+ -0.5) = "-(1+i)/2"
  toTypst (0 :+ a) = concat [toTypst a, "i"]
  toTypst (a :+ b)
    | b >= 0 = concat [toTypst a, " + ", toTypst b, "i"]
    | otherwise = concat [toTypst a, " - ", toTypst (-b), "i"]

instance Typst AExpr where
  toTypst (AEConj s) = "(" ++ toTypst s ++ ")^*"
  toTypst (AEVar s) = escapeVar s
  toTypst (AEVal i) = toTypst i
  toTypst (AEBOp OPlus xs) = "(" ++ intercalateMap " + " toTypst xs ++ ")"
  toTypst (AEBOp OTimes xs) = "(" ++ intercalateMap " times " toTypst xs ++ ")"

instance Typst SNFTerm where
  toTypst :: SNFTerm -> String
  toTypst (SNFTerm (1 :+ 0) []) = "1"
  toTypst (SNFTerm (1 :+ 0) vs) = intercalateMap " " escapeVar vs
  toTypst (SNFTerm (-0.5 :+ 0) vs) = "-(" ++ intercalateMap " " escapeVar vs ++ ")/2"
  toTypst (SNFTerm (0 :+ -0.5) vs) = "-(" ++ intercalateMap " " escapeVar vs ++ "i)/2"
  toTypst (SNFTerm (0.5 :+ 0) vs) = "(" ++ intercalateMap " " escapeVar vs ++ ")/2"
  toTypst (SNFTerm (0 :+ 0.5) vs) = "(" ++ intercalateMap " " escapeVar vs ++ "i)/2"
  toTypst (SNFTerm i vs) = toTypst i ++ intercalateMap " " escapeVar vs

instance Typst SNF where
  toTypst (SNF ts) = intercalateMap " + " toTypst ts

instance Typst Mat where
  toTypst (Mat xss) =
    "mat(" ++ intercalate " ; " [intercalateMap ", " toTypst row | row <- xss] ++ ")"

instance Typst EMat where
  toTypst (EMat xss) = "mat(" ++ intercalate "; " [intercalateMap ", " toTypst row | row <- xss] ++ ")"
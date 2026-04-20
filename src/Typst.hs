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
escapeVar s
  | length s == 1 = s
  | otherwise = "\"" ++ s ++ "\""

instance Typst AExpr where
  toTypst (AEVar s) = escapeVar s
  toTypst (AEVal i) = show i
  toTypst (AEBOp OPlus xs) = "(" ++ intercalateMap " + " toTypst xs ++ ")"
  toTypst (AEBOp OTimes xs) = "(" ++ intercalateMap " times " toTypst xs ++ ")"

instance Typst SNFTerm where
  toTypst :: SNFTerm -> String
  toTypst (SNFTerm (i :+ 0) []) = show i
  toTypst (SNFTerm i []) = show i
  toTypst (SNFTerm (1 :+ 0) vs) = intercalateMap " " escapeVar vs
  toTypst (SNFTerm (x :+ 0) vs) = show x ++ intercalateMap " " escapeVar vs
  toTypst (SNFTerm i vs) = show i ++ intercalateMap " " escapeVar vs

instance Typst SNF where
  toTypst (SNF ts) = intercalateMap " + " toTypst ts

-- instance Typst Mat where
--   toTypst (Mat xss) =
--     "mat(" ++ intercalate " ; " [intercalateMap ", " toTypst row | row <- xss] ++ ")"

instance Typst EMat where
  toTypst (EMat xss) = "mat(" ++ intercalate "; " [intercalateMap ", " toTypst row | row <- xss] ++ ")"
{-# LANGUAGE InstanceSigs #-}

module Algebra (Expr (EVar, ELit, EBOp), BOp (OPlus, OTimes), toSNF, SNF, SNFTerm, eval) where

import Data.Complex
import Data.List (intercalate, sort, sortOn)
import Data.Semigroup (Semigroup (sconcat))

type CR = Complex Double

-- Raw Expressions
data BOp = OPlus | OTimes

data Expr
  = EVar String
  | ELit CR
  | EBOp BOp [Expr]

instance Show Expr where
  show (EVar s) = s
  show (ELit i) = show i
  show (EBOp OPlus xs) = "(" ++ intercalate " + " (map show xs) ++ ")"
  show (EBOp OTimes xs) = "(" ++ intercalate " * " (map show xs) ++ ")"

--

-- Sum Normal form terms
data SNFTerm = SNFTerm CR [String]

instance Show SNFTerm where
  show (SNFTerm (i :+ 0) []) = show i
  show (SNFTerm i []) = show i
  show (SNFTerm (1 :+ 0) vs) = concat vs
  show (SNFTerm (x :+ 0) vs) = show x ++ concat vs
  show (SNFTerm i vs) = show i ++ concat vs

data SNF = SNF [SNFTerm]

instance Semigroup SNF where
  sconcat xs = SNF $ concatMap (\(SNF x) -> x) xs

instance Monoid SNF where
  mempty :: SNF
  mempty = SNF []

instance Show SNF where
  show (SNF xs) = intercalate " + " $ map show xs

multiply :: SNFTerm -> SNFTerm -> SNFTerm
multiply (SNFTerm ai avs) (SNFTerm bi bvs) = SNFTerm (ai * bi) (avs ++ bvs)

expand :: SNF -> SNF -> SNF
expand (SNF as) (SNF bs) = SNF [multiply a b | a <- as, b <- bs]

toSNF :: Expr -> SNF
toSNF (EVar s) = SNF [SNFTerm 1 [s]]
toSNF (ELit i) = SNF [SNFTerm i []]
toSNF (EBOp OPlus es) = foldMap toSNF es
toSNF (EBOp OTimes es) =
  let es' = map toSNF es
   in case es' of
        [] -> SNF []
        ([e]) -> e
        e : rest -> foldl expand e rest

normaliseSNFTerm :: SNFTerm -> SNFTerm
normaliseSNFTerm (SNFTerm i vs) = SNFTerm i $ sort vs

normaliseSNF :: SNF -> SNF
normaliseSNF (SNF s) =
  let normedList =
        filter (\(SNFTerm i _) -> i /= 0) $
          sortOn (\(SNFTerm _ xs) -> xs) $
            map normaliseSNFTerm s
   in SNF (if length normedList == 0 then [SNFTerm 0 []] else normedList)

collect :: [SNFTerm] -> [SNFTerm]
collect [] = []
collect [x] = [x]
collect (x@(SNFTerm i xs) : y@(SNFTerm j ys) : rest)
  | xs == ys = collect $ (SNFTerm (i + j) ys) : rest
  | otherwise = x : collect (y : rest)

toRSNF :: SNF -> SNF
toRSNF s =
  let (SNF p) = normaliseSNF s
   in SNF $ collect p

eval :: Expr -> SNF
eval = toRSNF . toSNF
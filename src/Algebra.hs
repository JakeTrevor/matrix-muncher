{-# LANGUAGE InstanceSigs #-}

module Algebra (AExpr (AEVar, AEVal, AEBOp), BOp (OPlus, OTimes), toSNF, SNF (SNF), SNFTerm (SNFTerm), eval) where

import Data.Complex
import Data.List (sort, sortOn)
import Data.Semigroup (Semigroup (sconcat))
import Lib

type CR = Complex Double

-- Raw Expressions
data BOp = OPlus | OTimes

instance Show BOp where
  show OPlus = " + "
  show OTimes = " * "

-- 'Algebra' expression
data AExpr
  = AEVar String
  | AEVal CR
  | AEBOp BOp [AExpr]

instance Show AExpr where
  show (AEVar s) = s
  show (AEVal (i :+ 0)) = show i
  show (AEVal (0 :+ i)) = ":+" ++ show i
  show (AEVal i) = show i
  show (AEBOp _ []) = ""
  show (AEBOp op xs) = "(" ++ intercalateMap (show op) show xs ++ ")"

instance Num AExpr where
  (+) a b = AEBOp OPlus [a, b]
  (*) a b = AEBOp OTimes [a, b]
  abs _ = error "broke"
  signum _ = error "broke"
  negate x = AEBOp OTimes [-1, x]

  fromInteger x = AEVal (fromIntegral x :+ 0)

-- Sum Normal form terms
data SNFTerm = SNFTerm CR [String]

instance Show SNFTerm where
  show (SNFTerm (i :+ 0) []) = show i
  show (SNFTerm i []) = show i
  show (SNFTerm (1 :+ 0) vs) = concat vs
  show (SNFTerm (i :+ 0) vs) = show i ++ concat vs
  show (SNFTerm i vs) = show i ++ concat vs

data SNF = SNF [SNFTerm]

instance Semigroup SNF where
  sconcat xs = SNF $ concatMap (\(SNF x) -> x) xs

instance Monoid SNF where
  mempty :: SNF
  mempty = SNF []

instance Show SNF where
  show (SNF xs) = intercalateMap " + " show xs

multiply :: SNFTerm -> SNFTerm -> SNFTerm
multiply (SNFTerm ai avs) (SNFTerm bi bvs) = SNFTerm (ai * bi) (avs ++ bvs)

expand :: SNF -> SNF -> SNF
expand (SNF as) (SNF bs) = SNF [multiply a b | a <- as, b <- bs]

toSNF :: AExpr -> SNF
toSNF (AEVar s) = SNF [SNFTerm 1 [s]]
toSNF (AEVal i) = SNF [SNFTerm i []]
toSNF (AEBOp OPlus es) = foldMap toSNF es
toSNF (AEBOp OTimes es) =
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

-- Reduced SNF - i.e. after collecting like terms
toRSNF :: SNF -> SNF
toRSNF s =
  let (SNF p) = normaliseSNF s
   in SNF $ collect p

eval :: AExpr -> SNF
eval = toRSNF . toSNF

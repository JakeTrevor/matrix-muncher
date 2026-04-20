module Permutation (permGate, permGateInv) where

import Data.List (delete, elemIndex)
import Matrix
import MatrixAlgebra

complete :: Int -> [Int] -> [Int]
complete x is = is ++ [j | j <- [1 .. x], (j `notElem` is)]

swapGate :: MExpr
swapGate =
  MEVal $
    Mat
      [ [1, 0, 0, 0],
        [0, 0, 1, 0],
        [0, 1, 0, 0],
        [0, 0, 0, 1]
      ]

idGate :: Int -> MExpr
idGate x = MEVal $ idMat (2 ^ x)

swapAdj :: Int -> Int -> MExpr
swapAdj dim target = (idGate target) *^* swapGate *^* idGate (dim - target - 2)

shiftRight :: Int -> Int -> MExpr
shiftRight l dim = MEOp MatTimes [swapAdj dim i | i <- [l .. dim]]

unsafeIndex :: (Eq a) => a -> [a] -> Int
unsafeIndex x xs = case x `elemIndex` xs of
  Nothing -> error "Could not find x in xs"
  Just i -> i

permGateInner :: Int -> [Int] -> MExpr
permGateInner 1 [0] = MEVal $ idMat 2
permGateInner x is =
  let m = foldl1 max is
      idx = m `unsafeIndex` is
      rest = delete m is
   in ((permGateInner (x - 1) rest) *^* (idGate 1)) * (shiftRight idx m)

permGate :: Int -> [Int] -> Mat
permGate x is = evalMExpr $ permGateInner x $ complete x is

permGateInv :: Int -> [Int] -> Mat
permGateInv x is = matTrans $ permGate x is
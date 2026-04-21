module Permutation (idGate, permGate, permGateInv) where

import Data.List (delete, elemIndex)
import Debug.Trace (trace, traceShow, traceShowId)
import Matrix
import MatrixAlgebra

complete :: Int -> [Int] -> [Int]
complete x is = is ++ [j | j <- [0 .. (x - 1)], (j `notElem` is)]

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
idGate x
  | x >= 0 = MEVal $ idMat (2 ^ x)
  | otherwise = error "cannot construct matrix"

swapAdj :: Int -> Int -> MExpr
swapAdj dim target =
  if (dim - (target + 2)) > 0
    then (idGate target) *^* swapGate *^* idGate (dim - (target + 2))
    else traceShow target (idGate target) *^* swapGate

shiftRight :: Int -> Int -> MExpr
shiftRight dim target = MEOp MatTimes [swapAdj dim i | i <- [target .. (dim - 1)]]

unsafeIndex :: (Eq a) => a -> [a] -> Int
unsafeIndex x xs = case x `elemIndex` xs of
  Nothing -> error "Could not find x in xs"
  Just i -> i

permGateInner :: Int -> [Int] -> MExpr
permGateInner 0 _ = error "unreachable"
permGateInner 1 [0] = trace "reached the base case" MEVal $ idMat 2
permGateInner 1 _ = error "unreachable"
permGateInner x is =
  let m = maximum is
      idx = m `unsafeIndex` is
      rest = delete m is
   in ((permGateInner (x - 1) rest) *^* (idGate 1)) * (shiftRight m idx)

permGate :: Int -> [Int] -> Mat
permGate x is = traceShowId $ evalMExpr $ traceShowId $ permGateInner x $ complete x is

permGateInv :: Int -> [Int] -> Mat
permGateInv x is = matTrans $ permGate x is
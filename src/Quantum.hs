module Quantum (controlled, copies) where

import Matrix
import MatrixAlgebra (MExpr (MEExprOp, MEOp, MEVal), MatOp (MatKronecker))

controlled :: Mat -> Mat
controlled m
  | square m =
      let (s, _) = size m
       in fromBlocks [[idMat s, zeroMat s], [zeroMat s, m]]
  | otherwise = error "Cannot control a non-square matrix"

copies :: Int -> MExpr -> MExpr
-- copies 0 _ = MEVal $ [1]
copies n x = MEOp MatKronecker $ replicate n x
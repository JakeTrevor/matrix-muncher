module Quantum (controlled) where

import Matrix

controlled :: Mat -> Mat
controlled m
  | square m =
      let (s, _) = size m
       in fromBlocks [[idMat s, zeroMat s], [zeroMat s, m]]
  | otherwise = error "Cannot control a non-square matrix"

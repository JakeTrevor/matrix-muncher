-- module Permutation () where

-- import Matrix

-- complete :: Int -> [Int] -> [Int]
-- complete x is = is ++ [j | j <- [1 .. x], (j `notElem` is)]

-- shiftRight l u = error "not implemented"

-- gatePerm :: Int -> [Int] -> Mat
-- gatePerm x is =
--   let m = max is
--       idx = indexOf m is
--       rest = remove m is
--    in (kronecker (gatePerm (x - 1) rest) (idMat 2)) `matMul` shiftRight idx m

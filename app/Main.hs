module Main (main) where

import Algebra
import Data.List (intercalate, transpose)
import Lib (ShowLines (toLines))
import Matrix
import MatrixAlgebra
import Quantum
import Typst

testExpr :: AExpr
testExpr = (AEVar "x" + AEVar "x") * (AEVar "a" + AEVar "b" + AEVar "c") * AEVar "z"

testExpr2 :: AExpr
testExpr2 = (1 + 2) * (3 + 4 + 5) * (6)

testMat :: MExpr
testMat = MEVal $ Mat [[AEVar "a", AEVar "b"], [AEVar "c", AEVar "d"]]

testMat2 :: MExpr
testMat2 = MEVal $ Mat [[AEVar "x", AEVar "y"], [AEVar "k", AEVar "j"]]

xMat :: Mat
xMat = Mat $ [[0, 1], [1, 0]]

zMat :: Mat
zMat = Mat $ [[1, 0], [0, -1]]

eqls :: [String]
eqls = transpose ["    ", " =  ", "    "]

rt2 :: AExpr
rt2 = AEVal $ (sqrt 2) / 2

-- printMat :: Mat -> IO ()
-- printMat = putStrLn . show . evalMat

-- signalBits :: Int -> Int

-- messageBit :: [Int]
-- messageBit = [1]

-- signalBits :: Int -> [Int]
-- signalBits n = take n [2, 4 ..]

-- perm :: Int -> [Int] -> Mat
-- perm = error "not implemented"

-- invPerm :: Int -> [Int] -> Mat
-- invPerm i xs = matTrans $ perm i xs

-- enc :: Int -> Mat
-- enc n =
--   let rt2_id = scalarMul rt2 $ idMat 2
--       x_component = idMat 2
--       z_component = idMat 2
--       p = perm (2 * n + 1) (messageBit ++ (signalBits n))
--       p_inv = invPerm (2 * n + 1) (messageBit ++ signalBits n)
--    in p_inv @ (rt2_id `matPlus` x_component) @ (rt2_id `matPlus` z_component) @ p

main :: IO ()
main = do
  putStrLn "hello"
  putStrLn $ show testMat
  putStrLn $ show ((testMat + testMat2) * testMat)
  putStrLn $ show $ munch ((testMat + testMat2) * testMat)
module Main (main) where

import Algebra
import Data.List (intercalate, transpose)
import Matrix

testExpr :: Expr
testExpr = EBOp OTimes [(EBOp OPlus [EVar "x", EVar "x"]), (EBOp OPlus [EVar "a", EVar "b", EVar "c"]), EVar "z"]

testExpr2 :: Expr
testExpr2 = EBOp OTimes [(EBOp OPlus [ELit 1, ELit 2]), (EBOp OPlus [ELit (3), ELit (4), ELit (5)]), ELit (6)]


testMat :: Mat
testMat = Mat [[EVar "a", EVar "b"], [EVar "c", EVar "d"]]

testMat2 :: Mat
testMat2 = Mat [[EVar "x", EVar "y"], [EVar "k", EVar "j"]]


eqls :: [String]
eqls = transpose ["    ", " =  ", "    "]

main :: IO ()
main = do
  putStrLn ""
  putStr "Test Expression: "
  putStrLn (show testExpr)
  putStr "in raw SNF:      "
  putStrLn (show $ toSNF testExpr)
  putStr "in RSNF:         "
  putStrLn (show $ eval testExpr)
  let testMatRows = formatRows testMat
  let testMat2Rows = formatRows testMat2
  let resultRows = formatRows $ matMul testMat testMat2
  let toPrint = [testMatRows, testMat2Rows, eqls, resultRows]
  putStrLn $ intercalate "\n" $ map concat (transpose toPrint)

  putStrLn $ show $ idMat 4
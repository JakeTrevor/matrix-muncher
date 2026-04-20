module MatrixAlgebra (MExpr (MEVal, MEOp, MEExprOp, MEBlocks), MatOp (MatPlus, MatTimes, MatKronecker), evalMExpr, munch, EMat (EMat), (*^*)) where

import Algebra
import Data.List (intercalate, intersperse, transpose)
import Lib
import Matrix

data MatOp = MatPlus | MatTimes | MatKronecker

instance Show MatOp where
  show MatPlus = " + "
  show MatTimes = " ◦ "
  show MatKronecker = " ⊗ "

data MExpr
  = MEVal Mat
  | -- | MEVar String  -- these are really hard, so the're turned off for now
    MEOp MatOp [MExpr]
  | MEExprOp BOp AExpr MExpr
  | MEBlocks [[MExpr]]

instance Num MExpr where
  (+) a b = MEOp MatPlus [a, b]
  (*) a b = MEOp MatTimes [a, b]
  negate = MEExprOp OTimes (-1)
  abs = error "Not implemented"
  signum = error "Not implemented"
  fromInteger = error "Not Implemented"

(*^*) :: MExpr -> MExpr -> MExpr
a *^* b = MEOp MatKronecker [a, b]

instance ShowLines MExpr where
  toLines (MEVal m) = toLines m
  -- toLines (MEVar x) = [x]
  toLines (MEOp op ms) =
    let exprLns = map toLines ms
        opLn = [show op]
        allLns = intersperse opLn exprLns
        allLns' = transpose $ joinLines $ alignLines allLns
        allLns'' = map (\x -> "│" ++ x ++ "│") allLns'
        spaces = mkSpaces $ allLns' !! 0
     in ("╭" ++ spaces ++ "╮") : allLns'' ++ [("╰" ++ spaces ++ "╯")]
  toLines (MEExprOp op s ms) =
    let exprLn = [show s]
        opLn = [show op]
        s2 = toLines ms
     in joinLines $ alignLines [exprLn, opLn, s2]
  toLines (MEBlocks ms) = error "Don't ask me to print from blocks"

instance Show MExpr where
  show m = intercalate "\n" $ toLines m

data EMat = EMat [[SNF]]

evalMExpr :: MExpr -> Mat
evalMExpr (MEVal x) = x
evalMExpr (MEOp MatPlus xs) = foldl1 matPlus (map evalMExpr xs)
evalMExpr (MEOp MatTimes xs) = foldl1 matMul (map evalMExpr xs)
evalMExpr (MEOp MatKronecker xs) = foldl1 kronecker (map evalMExpr xs)
evalMExpr (MEExprOp OPlus e x) = scalarAdd e $ evalMExpr x
evalMExpr (MEExprOp OTimes e x) = scalarMul e $ evalMExpr x
evalMExpr (MEBlocks xs) = fromBlocks $ map (map evalMExpr) xs

evalMat :: Mat -> EMat
evalMat (Mat m) = EMat $ map (map eval) m

munch :: MExpr -> EMat
munch = evalMat . evalMExpr

instance ShowLines EMat where
  toLines (EMat m) =
    let columns = transpose $ map ((mapBut1 (++ ", ")) . map show) m
        paddedColumns = map padToMax columns
        rows = map (intercalate "") $ transpose paddedColumns
        lineWidth = length $ rows !! 0
        spaces = take lineWidth $ cycle " "
        wrappedRows = map (\x -> "│" ++ x ++ "│") rows
     in ("┌" ++ spaces ++ "┐") : wrappedRows ++ ["└" ++ spaces ++ "┘"]

instance Show EMat where
  show m1@(EMat m)
    | length m < 1 = "[" ++ (intercalate "\n" $ map (intercalate ", " . map show) m) ++ "]"
    | otherwise = intercalate "\n" $ toLines m1
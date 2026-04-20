module Matrix (Mat (Mat), size, square, matRows, matCols, matPlus, matMul, scalarAdd, scalarMul, matTrans, kronecker, fromBlocks) where

import Algebra
import Data.List (intercalate, transpose)
import Data.Monoid (All (All, getAll))
import Lib

data Mat = Mat [[AExpr]]

instance ShowLines Mat where
  toLines (Mat m) =
    let rowsRaw = map ((mapBut1 (++ ", ")) . map show) m
        columnsRaw = transpose rowsRaw
        paddedColumns = map padToMax columnsRaw
        rows = map (intercalate "") $ transpose paddedColumns
        spaces = mkSpaces $ rows !! 0
        wrappedRows = map (\x -> "│" ++ x ++ "│") rows
     in ("┌" ++ spaces ++ "┐") : wrappedRows ++ ["└" ++ spaces ++ "┘"]

instance Show Mat where
  show m = intercalate "\n" $ toLines m

wellFormed :: Mat -> Bool
wellFormed (Mat []) = True
wellFormed (Mat (r : rs)) = getAll (foldMap (\a -> All (length a == length r)) rs)

size :: Mat -> (Int, Int)
size (Mat []) = (0, 0)
size m@(Mat (r : rs))
  | wellFormed m = (length rs + 1, length r)
  | otherwise = error "ill-formed matrix"

matRows :: Mat -> Int
matRows = fst . size

matCols :: Mat -> Int
matCols = snd . size

square :: Mat -> Bool
square m = let (x, y) = size m in x == y

matPlus :: Mat -> Mat -> Mat
matPlus m1@(Mat xs) m2@(Mat ys)
  | size m1 /= size m2 = error "cannot add matrices of different sizes"
  | otherwise =
      let zipped = map (uncurry zip) (zip xs ys)
          zopped = map (map (AEBOp OPlus . pairToList)) zipped
       in Mat zopped

matMul :: Mat -> Mat -> Mat
matMul m@(Mat m1) n@(Mat m2)
  | matRows m == matCols n = Mat [[AEBOp OPlus (map (AEBOp OTimes . pairToList) (zip col row)) | col <- transpose m1] | row <- m2]
  | otherwise = error "Dimensions are wrong - Matrix multiplication is ill defined"

scalarMul :: AExpr -> Mat -> Mat
scalarMul e (Mat m) = Mat $ map (map (\e' -> AEBOp OTimes [e, e'])) m

scalarAdd :: AExpr -> Mat -> Mat
scalarAdd e (Mat m) = Mat $ map (map (\e' -> AEBOp OPlus [e, e'])) m

matTrans :: Mat -> Mat
matTrans (Mat m) = Mat $ transpose m

concatRows :: Mat -> Mat -> Mat
concatRows (Mat m1) (Mat m2) = Mat $ map (uncurry (++)) $ zip m1 m2

concatCols :: Mat -> Mat -> Mat
concatCols (Mat m1) (Mat m2) = Mat $ transpose $ map (uncurry (++)) $ zip (transpose m1) (transpose m2)

fromBlocks :: [[Mat]] -> Mat
fromBlocks xss = foldr1 concatCols (map (foldr1 concatRows) xss)

kronecker :: Mat -> Mat -> Mat
kronecker (Mat a) m = fromBlocks $ map (map (\x -> scalarMul x m)) a

---

-- idMat :: Int -> Mat
-- idMat x
--   | x >= 0 = Mat [[ELit (if i == j then 1 else 0) | i <- [1 .. x]] | j <- [1 .. x]]
--   | otherwise = error "Cannot construct a negative-sized matrix"

-- zeroMat :: Int -> Mat
-- zeroMat x
--   | x >= 0 = Mat [[ELit 0 | _ <- [1 .. x]] | _ <- [1 .. x]]
--   | otherwise = error "Cannot construct a negative-sized matrix"

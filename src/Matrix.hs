module Matrix (Mat, idMat, zeroMat, size, square, fromBlocks, matTrans, matPlus, matMul, formatRows) where

import Algebra
import Data.List (intercalate, transpose)
import Data.Monoid (All (All, getAll))

data Mat = Mat [[Expr]]

pad :: [a] -> Int -> [a] -> [a]
pad chr sz str = take (sz) (str ++ (cycle chr))

padToMax :: [String] -> [String]
padToMax strs =
  let maxWidth = foldl1 max (map length strs)
   in map (pad " " maxWidth) strs

mapBut1 :: (a -> a) -> [a] -> [a]
mapBut1 _ [] = []
mapBut1 _ [x] = [x]
mapBut1 f (x : xs) = f x : mapBut1 f xs

formatRows :: Mat -> [String]
formatRows (Mat m) =
  let columns = transpose $ map ((mapBut1 (++ ", ")) . map (show . eval)) m
      paddedColumns = map padToMax columns
      rows = map (intercalate "") $ transpose paddedColumns
      lineWidth = length $ rows !! 1
      spaces = take lineWidth $ cycle " "
      wrappedRows = map (\x -> "│" ++ x ++ "│") rows
   in ("┌" ++ spaces ++ "┐") : wrappedRows ++ ["└" ++ spaces ++ "┘"]

instance Show Mat where
  show m1@(Mat m)
    | length m < 1 = "[" ++ (intercalate "\n" $ map (intercalate ", " . map (show . eval)) m) ++ "]"
    | otherwise = intercalate "\n" $ formatRows m1

wellFormed :: Mat -> Bool
wellFormed (Mat []) = True
wellFormed (Mat (r : rs)) = getAll (foldMap (\a -> All (length a == length r)) rs)

size :: Mat -> (Int, Int)
size (Mat []) = (0, 0)
size m@(Mat (r : rs))
  | wellFormed m = (length rs + 1, length r)
  | otherwise = error "ill-formed matrix"

square :: Mat -> Bool
square m = let (x, y) = size m in x == y

matPlus :: Mat -> Mat -> Mat
matPlus m1@(Mat xs) m2@(Mat ys)
  | size m1 /= size m2 = error "cannot add matrices of different sizes"
  | otherwise =
      let zipped = map (uncurry zip) (zip xs ys)
          zopped = map (map (\(e1, e2) -> EBOp OPlus [e1, e2])) zipped
       in Mat zopped

matMul :: Mat -> Mat -> Mat
matMul m@(Mat m1) n@(Mat m2) =
  Mat [[EBOp OPlus (map (\(e1, e2) -> EBOp OTimes [e1, e2]) (zip col row)) | col <- transpose m1] | row <- m2]

matTrans :: Mat -> Mat
matTrans (Mat m) = Mat $ transpose m

idMat :: Int -> Mat
idMat x
  | x >= 0 = Mat [[ELit (if i == j then 1 else 0) | i <- [1 .. x]] | j <- [1 .. x]]
  | otherwise = error "Cannot construct a negative-sized matrix"

zeroMat :: Int -> Mat
zeroMat x
  | x >= 0 = Mat [[ELit 0 | _ <- [1 .. x]] | _ <- [1 .. x]]
  | otherwise = error "Cannot construct a negative-sized matrix"

fromBlocks :: [[Mat]] -> Mat
fromBlocks xss = error "not implemented yet"

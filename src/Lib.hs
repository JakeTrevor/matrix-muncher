module Lib (intercalateMap, ShowLines (toLines), alignLines, joinLines, padToMax, mapBut1, pairToList, mkSpaces) where

import Data.List (intercalate, transpose)

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

intercalateMap :: [a] -> (b -> [a]) -> [b] -> [a]
intercalateMap x f ys = intercalate x $ map f ys

class ShowLines t where
  toLines :: t -> [String]

mkSpaces :: String -> String
mkSpaces s = replicate (length s) ' '

padL :: Int -> a -> [a] -> [a]
padL n x xs = replicate n x ++ xs

padR :: Int -> a -> [a] -> [a]
padR n x xs = xs ++ replicate n x

padEven :: Int -> a -> [a] -> [a]
padEven n x xs = replicate lpad x ++ xs ++ replicate rpad x
  where
    lpad = n `div` 2
    rpad = n - lpad

ensureWidthL :: Int -> a -> [a] -> [a]
ensureWidthL n x xs = padL lpad x xs
  where
    lpad = n - length xs

ensureWidthR :: Int -> a -> [a] -> [a]
ensureWidthR n x xs = padR rpad x xs
  where
    rpad = n - length xs

ensureWidthEven :: Int -> a -> [a] -> [a]
ensureWidthEven n x xs = padEven padding x xs
  where
    padding = n - length xs

padOut :: [String] -> [String]
padOut xs = map (ensureWidthEven maxLen ' ') xs
  where
    maxLen = foldl1 max $ map length xs

alignLines :: [[String]] -> [[String]]
alignLines xs =
  let tallest = foldl1 max $ map length xs
   in map padOut $ map (ensureWidthEven tallest " ") xs

joinLines :: [[String]] -> [String]
joinLines p = transpose $ map (concat) $ transpose p

padToMax :: [String] -> [String]
padToMax strs =
  let maxWidth = foldl1 max (map length strs)
   in map (ensureWidthR maxWidth ' ') strs

mapBut1 :: (a -> a) -> [a] -> [a]
mapBut1 _ [] = []
mapBut1 _ [x] = [x]
mapBut1 f (x : xs) = f x : mapBut1 f xs

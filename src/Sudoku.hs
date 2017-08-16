module Sudoku where

import Data.List

-- A single entry in the sudoku, usually empty or 1-9.
data Entry a = Empty | Entry a

instance (Show a) => Show (Entry a) where
  show Empty = "-"
  show (Entry a) = "\ESC[32m" ++ show a ++ "\ESC[m"

instance (Eq a) => Eq (Entry a) where
  Empty == Empty = True
  Empty == _ = False
  _ == Empty = False
  (Entry x) == (Entry y) = x == y

type Row a = [a]
type Column a = [a]
type Section a = [a]
newtype Sudoku a =  Sudoku [[a]]

cols :: Sudoku a -> [Column a]
cols (Sudoku columns) = columns

rows :: Sudoku a -> [Row a]
rows (Sudoku columns) = transpose columns

secs :: Sudoku a -> [Section a]
secs sudoku = concatMap extractSections threeByThree
  where threeByThree = inGroupsOf 3 $ map (inGroupsOf 3) $ cols sudoku
        extractSections [] = []
        extractSections [xs] = xs
        extractSections [xs, ys] = xs ++ ys
        extractSections (xs:ys:zs:_) = zipWith3 concat3 xs ys zs
        concat3 a b c = a ++ b ++ c

stringify :: Show a => Sudoku a -> Sudoku String
stringify = fmap show

instance (Show a) => Show (Sudoku a) where
  show sudoku = unlines stringifiedRows
    where stringifiedRows = map unwords $ rows stringified
          stringified = stringify sudoku

instance Functor Sudoku where
  fmap f (Sudoku columns) = Sudoku $  fmap (fmap f) columns

flatten :: Sudoku a -> [a]
flatten (Sudoku columns) = foldl1 (++) columns

instance Foldable Sudoku where
  foldr f initial sudoku = foldr f initial $ flatten sudoku

get :: Int -> Int -> Sudoku a -> a
get x y (Sudoku columns) = column !! y
  where column = columns !! x

setAt :: Int -> a -> [a] -> [a]
setAt i x xs
  | i < 0 = xs
  | i >= length xs = xs
  | otherwise = pre ++ [x] ++ post
            where (pre, _:post) = splitAt i xs

set :: Int -> Int -> a -> Sudoku a -> Sudoku a
set x y element (Sudoku columns)
  | x < 0 = Sudoku columns
  | x > length columns = Sudoku columns
  | otherwise = Sudoku $ pre ++ [setAt y element col] ++ post
              where (pre, col:post) = splitAt x columns


-- Validation of sudoku

inGroupsOf :: Int -> [a] -> [[a]]
inGroupsOf _ [] = []
inGroupsOf n xs
  | n > 0    = grouped : inGroupsOf n ungrouped
  | otherwise = []
  where (grouped, ungrouped) = splitAt n xs

validEntries :: Eq a => [Entry a] -> Bool
validEntries entries = filled == nub filled
  where filled = filter (Empty /=) entries

validRows :: Eq a => Sudoku (Entry a) -> Bool
validRows = all validEntries . rows

validCols :: Eq a => Sudoku (Entry a) -> Bool
validCols = all validEntries . cols

validSecs ::Eq a => Sudoku (Entry a) -> Bool
validSecs = all validEntries . secs

valid :: Eq a => Sudoku (Entry a) -> Bool
valid s = validRows s && validCols s && validSecs s

completed :: Eq a => Sudoku (Entry a) -> Bool
completed sudoku = valid sudoku && notElem Empty sudoku

module Sudoku (printSolutions) where

import Data.List (transpose, nub)

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char

boxsize               :: Int
boxsize               =  3

values                :: [Value]
values                =  ['1'..'9']

empty                 :: Value -> Bool
empty                 =  (== '.')

single                :: [a] -> Bool
single [_]            =  True
single _              =  False

easy                  :: Grid
easy                  =  ["2....1.38",
                          "........5",
                          ".7...6...",
                          ".......13",
                          ".981..257",
                          "31....8..",
                          "9..8...2.",
                          ".5..69784",
                          "4..25...."]

--First gentle example from sudoku.org.uk:
gentle                :: Grid
gentle                =  [".1.42...5",
                          "..2.71.39",
                          ".......4.",
                          "2.71....6",
                          "....4....",
                          "6....74.3",
                          ".7.......",
                          "12.73.5..",
                          "3...82.7."]

--First diabolical example:
diabolical            :: Grid
diabolical            =  [".9.7..86.",
                          ".31..5.2.",
                          "8.6......",
                          "..7.5...6",
                          "...3.7...",
                          "5...1.7..",
                          "......1.9",
                          ".2.6..35.",
                          ".54..8.7."]

--First "unsolvable" (requires backtracking) example:
unsolvable            :: Grid
unsolvable            =  ["1..9.7..3",
                          ".8.....7.",
                          "..9...6..",
                          "..72.94..",
                          "41.....95",
                          "..85.43..",
                          "..3...7..",
                          ".5.....4.",
                          "2..8.6..9"]

--Minimal sized grid (17 values) with a unique solution:
minimal               :: Grid
minimal               =  [".98......",
                          "....7....",
                          "....15...",
                          "1........",
                          "...2....9",
                          "...9.6.82",
                          ".......3.",
                          "5.1......",
                          "...4...2."]

--Empty grid:
blank                 :: Grid
blank                 =  replicate n (replicate n '.')
                         where n = boxsize ^ (2 :: Int)

--------------------------------------------------------------------------------

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

boxs                  :: Matrix a -> [Row a]
boxs                  =  unpack . map cols . pack
                         where
                            pack   = split . map split
                            split  = chop boxsize
                            unpack = map concat . concat

chop                  :: Int -> [a] -> [[a]]
chop _ []             =  []
chop n xs             =  take n xs : chop n (drop n xs)

valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs g)

nodups :: Eq a => [a] -> Bool
nodups []     = True
nodups (x:xs) = notElem x xs && nodups xs

--------------------------------------------------------------------------------

solve :: Grid -> [Grid]
solve = filter valid . collapse . choices

type Choices = [Value]

choices :: Grid -> Matrix Choices
choices = map (map choice) where
    choice c | c == '.'  =  ['1'..'9']
             | otherwise =  [c]

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs : xss) = [y : ys | y <- xs, ys <- cartesianProduct xss]

-- todo convince yourself that it works
collapse :: Matrix [a] -> [Matrix a]
collapse m = cartesianProduct (map cartesianProduct m)

--------------------------------------------------------------------------------

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows where
        pruneBy f = f . map reduce . f

fix :: Eq a => (a -> a) -> a -> a
fix f a =
  if a == a' then
    a
  else
    fix f a'
  where a' = f a

-- reduce ["1234", "1", "34", "3"] -> ["24", "1", "3", "3"]
reduce :: Row Choices -> Row Choices
reduce = fix impl where
  impl choices =
    let
      singleDigits = concat . nub . filter (\c -> length c == 1) $ choices
      excludeSingleDigits c =
        if length c == 1 then c
        else filter (`notElem` singleDigits) c
    in map excludeSingleDigits choices

solve2 :: Grid -> [Grid]
solve2 = filter valid . collapse . prune . choices

solve3 = filter valid . collapse . fix prune . choices

------------------------------------------------------------------

printSolutions :: IO ()
printSolutions = do
  let solutions = solve3 easy
  putStrLn $ "Solutions:" ++ show solutions
-- This is the first tempt to solve a sudoku problem using haskell.
-- Although this solves theoretically the problem, it is not the best solution.


import Data.List
-- Type declarations: The types below are used to represent the sudoku problem.

-- A Grid is composed of a list of lists of Matrixes.
type Grid = Matrix Value
-- A Matrix is composed of a list of lists of Rows.
type Matrix a = [Row a]
-- A Row is composed of a list of lists of Values.
type Row a = [a]
-- A Value is any Char. Although this could be a integer, we use Char to make easy for printing solution.
type Value = Char

type Choices = [Value]

-- Haskell treat everything as functions. There's no variable in Haskell, but functions which return values

-- Functions are created based on three steps:
-- 1. Function declarations: function name :: function type
-- 2. Function implementation: function name = function logic
-- 3. Function call: function name params

boxsize :: Int
boxsize = 3

values :: [Value]
values = ['1'..'9']

empty :: Value -> Bool
empty = (== '.')

single :: [a] -> Bool
single [_] = True
single _ = False

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

boxs :: Matrix a -> [Row a]
boxs =  unpack . map cols . pack   -- this is the same as unpack(map(cols(pack))) and is called functions composition
        where
            pack   = split . map split
            split = chop boxsize
            unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs g)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = notElem x xs && nodups xs

choices :: Grid -> Matrix Choices
choices g = map (map choice) (rows g)
            where
                choice v = if empty v then values else [v]

cp :: [[a]] -> [[a]]
cp [] =  [[]]
cp (xs:xss) = [y:ys | y <- xs, ys <- cp xss]

collapse :: Matrix [a] -> [Matrix a]
collapse = cp . map cp

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows
        where pruneBy f = f . map reduce . f

reduce :: Row Choices -> Row Choices
reduce xss =  [xs `minus` singles | xs <- xss]
                where singles = concat (filter single xss)

minus :: Choices -> Choices -> Choices
xs `minus` ys = if single xs then xs else xs \\ ys

solve2 :: Grid -> [Grid]
solve2 = filter valid . collapse . prune . choices
module Day3 where

import Universum

data Square = Open | Tree deriving (Show, Eq)
type Grid = [[Square]]

part1 :: IO Int
part1 = applySlope 1 3 <$> input

part2 :: IO Int
part2 = product . applySlopes <$> input
 where
  slopes = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
  applySlopes grid = ($ grid) . uncurry applySlope <$> slopes

applySlope :: Int -> Int -> Grid -> Int
applySlope down right g = go right $ drop down g
 where
  go _ [] = 0
  go right' (row : grid) =
    let (h : _) = drop right' row
    in (if h == Tree then 1 else 0) + go (right' + right) (drop (down - 1) grid)

input :: IO Grid
input = map parseRow . lines <$> readFile "input/day3.txt"
 where
  parseRow :: Text -> [Square]
  parseRow line =
    let row = parseSquare <$> toList line
    in  cycle row
  parseSquare '#' = Tree
  parseSquare _   = Open

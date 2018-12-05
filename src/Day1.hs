module Day1 where

import qualified Data.Set                      as S

day1Data :: IO [Integer]
day1Data = fmap (read . trimPlus) . lines <$> readFile "input/day1.txt"
 where
  trimPlus ('+' : ds) = ds
  trimPlus ds         = ds

solveDay1 :: IO ()
solveDay1 = day1Data >>= print . sum

solveDay1Part2 :: IO ()
solveDay1Part2 = do
  input <- day1Data
  print $ dupl S.empty $ scanl (+) 0 (cycle input)
 where
  dupl s (i : is) = if i `S.member` s then i else dupl (i `S.insert` s) is


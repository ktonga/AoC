module Day1 where

import Universum

part1 :: IO (Maybe Int)
part1 =  fmap snd . find ((== 2020) . fst) . (f <=< tails) <$> input
 where
  f (h : t) = (\x -> (h + x, h * x)) <$> t
  f _ = []

part2 :: IO (Maybe Int)
part2 =  fmap snd . find ((== 2020) . fst) . (f <=< tails) <$> input
 where
  f (h : t) = f' h <=< tails $ t
  f _ = []
  f' h (h' : t) = (\x -> (h + h' + x, h * h' * x)) <$> t
  f' _ _ = []

input :: IO [Int]
input = mapMaybe (readMaybe . toString) . lines <$> readFile "input/day1.txt"

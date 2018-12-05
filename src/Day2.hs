{-# LANGUAGE TupleSections #-}

module Day2 where

import           Data.Bool                      ( bool )
import           Data.Bifunctor                 ( bimap )
import           Data.Foldable                  ( fold
                                                , find
                                                )
import           Data.Monoid                    ( Sum(..) )
import           Data.List                      ( group
                                                , sort
                                                )

day2Data :: IO [String]
day2Data = lines <$> readFile "input/day2.txt"

solveDay2 :: IO ()
solveDay2 = day2Data >>= print . checksum
 where
  letterRepetition = fmap length . group . sort
  exactly xs = (2 `elem` xs, 3 `elem` xs)
  boolToSum = Sum . bool 0 1
  count = fold . fmap (bimap boolToSum boolToSum . exactly . letterRepetition)
  product (twos, threes) = twos * threes
  checksum = product . bimap getSum getSum . count

solveDay2Part2 :: IO ()
solveDay2Part2 = day2Data >>= putStrLn . removeDifferent . findDifferByOne
 where
  differByOne :: Int -> String -> String -> Bool
  differByOne d [] [] = d == 1
  differByOne 2 _  _  = False
  differByOne d (x : xs) (y : ys) =
    let d' = if x == y then d else d + 1 in differByOne d' xs ys

  findDifferByOne :: [String] -> (String, String)
  findDifferByOne (c : cs) =
    maybe (findDifferByOne cs) (c, ) $ find (differByOne 0 c) cs

  removeDifferent :: (String, String) -> String
  removeDifferent (s1, s2) = fst <$> filter (uncurry (==)) (zip s1 s2)

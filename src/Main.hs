{-# LANGUAGE LambdaCase #-}
module Main where

import           System.Environment             ( getArgs )
import qualified Data.Set                      as S

main :: IO ()
main =
  head
    <$> getArgs
    >>= (\case
          "d1"   -> readFile "input/day1.txt" >>= putStrLn . day1
          "d1p2" -> readFile "input/day1.txt" >>= putStrLn . day1part2
        )

parseDay1 :: String -> [Integer]
parseDay1 = fmap (read . trimPlus) . lines
 where
  trimPlus ('+' : ds) = ds
  trimPlus ds         = ds

day1 :: String -> String
day1 = show . sum . parseDay1

day1part2 :: String -> String
day1part2 str = show . dupl S.empty $ scanl (+) 0 input
 where
  input = parseDay1 str ++ input
  dupl s (i : is) = if i `S.member` s then i else dupl (i `S.insert` s) is


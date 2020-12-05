module Day2 where

import Universum hiding (many, (<|>))
import Universum.Unsafe ((!!))
import Text.Parsec
import Text.Parsec.Text

data Policy = Policy { pMin :: Int, pMax :: Int, pChar :: Char, pPass :: String } deriving (Show)

part1 :: IO Int
part1 = length . filter isValid <$> input
 where
  isValid p = 
    let charCount = length $ filter (== pChar p) $ toList $ pPass p
    in  charCount >= pMin p && charCount <= pMax p

part2 :: IO Int
part2 = length . filter isValid <$> input
  where isValid (Policy mi ma c p) = (p !! (mi - 1) == c) `xor` (p !! (ma - 1) == c)

input :: IO [Policy]
input = parseFromFile policies "input/day2.txt" >>= either (error . show) pure
 where
  policies = many policy
  policy =
    Policy
      <$> int <* char '-'
      <*> int <* spaces
      <*> anyChar <* char ':' <* spaces
      <*> restOfLine
  int = many digit >>= maybe (fail "NaN") pure . readMaybe
  restOfLine = manyTill anyChar (void newline <|> eof)

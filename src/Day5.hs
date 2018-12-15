module Day5 where

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( unless )
import           Data.Char                      ( isLower
                                                , toLower
                                                )
import           Data.Functor                   ( void )
import           Data.List                      ( sortOn )
import           Data.ListZipper

day5Data :: IO String
day5Data = head . lines <$> readFile "input/day5.txt"

trigger :: String -> String
trigger (p : ps) = execOpList' op (zipper0L p ps)
 where
  react p1 p2 = toLower p1 == toLower p2 && isLower p1 /= isLower p2
  op =
    getRightz
      >>= (\rs -> unless (null rs) $ do
            p1 <- getFocus
            p2 <- getRight
            if react p1 p2
              then
                void $ deleteStepRight >> (deleteStepLeft <|> deleteStepRight)
              else moveRight
            op
          )

solveDay5 :: IO ()
solveDay5 = day5Data >>= print . length . trigger

solveDay5Part2 :: IO ()
solveDay5Part2 = day5Data >>= print . selectBest . fullyReactAll
 where
  selectBest    = head . sortOn snd
  fullyReactAll = flip fmap ['a' .. 'z'] . fullyReact
  fullyReact input p = (p, length . trigger . removeProblematic p $ input)
  removeProblematic p = filter ((p /=) . toLower)


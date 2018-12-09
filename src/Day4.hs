module Day4 where

import           Data.Char                      ( isDigit )
import           Data.Foldable                  ( foldl'
                                                , foldl1
                                                )
import           Data.Function                  ( on )
import           Data.List                      ( intercalate
                                                , sort
                                                , sortOn
                                                , groupBy
                                                , maximumBy
                                                )
import           Data.List.Split
import           Data.Ord                       ( comparing )
import qualified Data.Set                      as S
import           Text.ParserCombinators.ReadP

type Minute  = Int
type GuardId = Int

data GuardEvent = BeginsShift GuardId
                | FallsAsleep
                | WakesUp
                deriving (Show)

data GuardRecord = GuardRecord Minute GuardEvent
                 deriving (Show)

parseGuardRecord :: String -> GuardRecord
parseGuardRecord = fst . head . readP_to_S guardRecordP
 where
  intP :: ReadP Int
  intP = read <$> munch1 isDigit

  idP :: ReadP GuardId
  idP = char '#' *> intP

  minuteP :: ReadP Int
  minuteP = skipMany (satisfy (/= ':')) *> get *> intP

  timeP :: ReadP Int
  timeP = between (char '[') (char ']') minuteP

  beginsShiftP :: ReadP GuardEvent
  beginsShiftP = BeginsShift <$> (string "Guard " *> idP)

  fallsAsleepP :: ReadP GuardEvent
  fallsAsleepP = FallsAsleep <$ string "falls asleep"

  wakesUpP :: ReadP GuardEvent
  wakesUpP = WakesUp <$ string "wakes up"

  eventP :: ReadP GuardEvent
  eventP = beginsShiftP <++ fallsAsleepP <++ wakesUpP

  guardRecordP :: ReadP GuardRecord
  guardRecordP = GuardRecord <$> timeP <* skipSpaces <*> eventP

day4Data :: IO [GuardRecord]
day4Data = fmap parseGuardRecord . sort . lines <$> readFile "input/day4.txt"

data Shift = Shift { shiftGuardId :: GuardId, shiftMinutes :: [Int] } deriving (Show)

mkShift :: [GuardRecord] -> Shift
mkShift (GuardRecord _ (BeginsShift id') : events) = Shift
  id'
  (minutes [] events)
 where
  minutes mins [] = mins ++ replicate (60 - length mins) 0
  minutes mins (GuardRecord m FallsAsleep : rest) =
    minutes (mins ++ replicate (m - length mins) 0) rest
  minutes mins (GuardRecord m WakesUp : rest) =
    minutes (mins ++ replicate (m - length mins) 1) rest

aggregateShiftsByGuard :: [Shift] -> [Shift]
aggregateShiftsByGuard = aggregate . grouped . sorted
 where
  sorted  = sortOn shiftGuardId
  grouped = groupBy ((==) `on` shiftGuardId)
  combine (Shift id' mins) (Shift _ mins') = Shift id' (zipWith (+) mins mins')
  aggregate = fmap (foldl1 combine)

type Strategy = [Shift] -> (GuardId, Int)

strategy1 :: Strategy
strategy1 = bestMinute . maxOn asleepMinutes
 where
  maxOn f = maximumBy (comparing f)
  asleepMinutes = sum . shiftMinutes
  bestMinute (Shift id' mins) = (id', fst $ maxOn snd ([0 ..] `zip` mins))

strategy2 :: Strategy
strategy2 = fmap fst . maxOn (snd . snd) . fmap bestMinute
 where
  maxOn f = maximumBy (comparing f)
  bestMinute (Shift id' mins) = (id', maxOn snd ([0 ..] `zip` mins))

solveDay4' :: Strategy -> IO ()
solveDay4' strategy =
  day4Data
    >>= print
    .   answer
    .   aggregateShiftsByGuard
    .   fmap mkShift
    .   recordsByGuard
 where
  isBegins (GuardRecord _ (BeginsShift _)) = True
  isBegins _                               = False
  recordsByGuard = split (dropInitBlank . keepDelimsL . whenElt $ isBegins)
  answer         = uncurry (*) . strategy

solveDay4 :: IO ()
solveDay4 = solveDay4' strategy1

solveDay4Part2 :: IO ()
solveDay4Part2 = solveDay4' strategy2


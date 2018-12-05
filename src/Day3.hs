module Day3 where

import           Data.Char                      ( isDigit )
import           Data.Foldable                  ( foldl' )
import           Data.List                      ( intercalate )
import qualified Data.Set                      as S
import           Text.ParserCombinators.ReadP

type ClaimId  = Int
type FromLeft = Int
type FromTop  = Int
type Width   = Int
type Height   = Int

data Claim = Claim ClaimId FromLeft FromTop Width Height deriving (Show)

parseClaim :: String -> Claim
parseClaim = fst . head . readP_to_S claimP
 where
  intP :: ReadP Int
  intP = read <$> munch1 isDigit

  idP :: ReadP ClaimId
  idP = char '#' *> intP

  leftTopP :: ReadP (FromLeft, FromTop)
  leftTopP = (,) <$> intP <* char ',' <*> intP

  weightHeightP :: ReadP (FromLeft, FromTop)
  weightHeightP = (,) <$> intP <* char 'x' <*> intP

  claimP :: ReadP Claim
  claimP = do
    id' <- idP
    string " @ "
    (l, t) <- leftTopP
    string ": "
    (w, h) <- weightHeightP
    pure $ Claim id' l t w h

day3Data :: IO [Claim]
day3Data = fmap parseClaim . lines <$> readFile "input/day3.txt"

data ClaimStatus = Unclaimed
                 | Claimed ClaimId
                 | Overlap [ClaimId]
                 deriving (Eq)

newtype Fabric = Fabric [[ClaimStatus]]

instance Show Fabric where
  show (Fabric rows) = intercalate "\n" (showCols <$> rows)
    where
      showCols = fmap showCell
      showCell Unclaimed = '.'
      showCell (Claimed id') = head $ show id'
      showCell (Overlap _) = 'X'

mkFabric :: Width -> Height -> Fabric
mkFabric w h = Fabric $ replicate h (replicate w Unclaimed)

claim :: Fabric -> Claim -> Fabric
claim (Fabric rows) (Claim id' l t w h) = Fabric (claimRows rows)
 where
  indexed = zip [0 ..]
  mapIfInRange f t g (i, a) = if i < f || i > t then a else g a
  claimRows = fmap (mapIfInRange t (t + h - 1) claimRow) . indexed
  claimRow  = fmap (mapIfInRange l (l + w - 1) claimCell) . indexed
  claimCell Unclaimed      = Claimed id'
  claimCell (Claimed id'') = Overlap [id', id'']
  claimCell (Overlap ids ) = Overlap (id' : ids)

claimAll :: IO Fabric
claimAll = claimAll' <$> day3Data
  where claimAll' = foldl' claim (mkFabric 1000 1000)

solveDay3 :: IO ()
solveDay3 = claimAll >>= print . countOverlaps
 where
  isOverlap (Overlap _) = True
  isOverlap _           = False
  countOverlaps (Fabric rows) = length $ filter isOverlap (concat rows)

solveDay3Part2 :: IO ()
solveDay3Part2 = claimAll >>= print . findNotOverlapping
 where
  f (once, overlap) status = case status of
    Claimed id' -> (S.insert id' once, overlap)
    Overlap ids -> (once, S.union (S.fromList ids) overlap)
    _           -> (once, overlap)
  claimedOnceVsOverlap :: Fabric -> (S.Set ClaimId, S.Set ClaimId)
  claimedOnceVsOverlap (Fabric rows) =
    foldl' f (S.empty, S.empty) (concat rows)
  doesNotOverlap (once, overlap) = S.difference once overlap
  findNotOverlapping = doesNotOverlap . claimedOnceVsOverlap


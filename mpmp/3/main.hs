{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Debug.Trace
import Text.Printf

-- the desired score
desiredScore = 46
-- the maximum hand length
expectedHandLength = 7

-- A tile is a letter
type Tile = Char

-- We use ' ' for blank, capital letters for the rest
tiles :: [Tile]
tiles = ' ' : ['A' .. 'Z']

-- Frequency table: how many tiles of each type there are
type FreqMap = Map Tile Int

freqTable :: FreqMap
freqTable = Map.fromList $ concat [singles, ones, twos, fours, sixes, nines]
  where
    ones = map (,1) "JKXQZ"
    twos = map (,2) "BCFHMPVWY " -- 2 blanks here
    fours = map (,4) "DLSU"
    sixes = map (,6) "NRT"
    nines = map (,9) "AI"
    singles = [('G', 3), ('O', 8), ('E', 12)]

-- Score table
type ScoreMap = Map Tile Int

scoreTable :: ScoreMap
scoreTable = Map.fromList $ concat [singles, ones, twos, threes, fours, eights, tens]
  where
    ones = map (,1) "AEILNORSTU"
    twos = map (,2) "DG"
    threes = map (,3) "BCMP"
    fours = map (,4) "FHVWY"
    eights = map (,8) "JX"
    tens = map (,10) "QZ"
    singles = [(' ', 0), ('K', 5)]

-- We can represent a hand as a list of tiles and then compute the length of
-- the list and its score by traversing it. But, since we also need to
-- determine how many tiles of each configuration are there, we instead
-- represent the hand as a map between tile and frequency. Moreover, for
-- efficiency reasons we also store number of tiles and score instead of
-- recalculating as needed. We also store the maximum letter just inserted, if
-- that exists so we don't keep retrying old letters.
data Hand = Hand
  { hand :: !FreqMap
  , length :: !Int
  , score :: !Int
  , maxTile :: !(Maybe Char)
  } deriving (Eq, Show)

-- empty hand
empty :: Hand
empty = Hand Map.empty 0 0 Nothing

-- tentatively adding a new tile to a hand
addTile :: Hand -> Tile -> Hand
addTile Hand{..} tile = Hand newHand newLength newScore newMax
  where
    newHand = Map.insertWith (+) tile 1 hand
    newLength = length + 1
    newScore =  score + scoreTable Map.! tile
    newMax = if maxTile == Nothing then Just tile else fmap (max tile) maxTile

-- are we done?
isDone :: Hand -> Bool
isDone Hand{..} = length == expectedHandLength

-- perfect score?
isGoodScore :: Hand -> Bool
isGoodScore Hand{..} = score == desiredScore

-- valid hand?
isValid :: Hand -> Bool
isValid Hand{..} = validLength && validScore && validComposition
  where
    validLength = length <= expectedHandLength
    validScore = score <= desiredScore
    validComposition = Map.isSubmapOfBy (<=) hand freqTable

-- finally, the search
search :: Hand -> [Hand]
search hand = do
  let tileSet = case maxTile hand of
        Nothing -> tiles
        Just ' ' -> tiles
        Just a  -> [a .. 'Z']
  newHand <- filter isValid $ map (addTile hand) tileSet
  if isDone newHand
  then if isGoodScore newHand then [newHand] else []
  else search newHand

-- the main of the executable
main = mapM_ print $ search empty

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Debug.Trace
import Text.Printf

-- Given a scrabble board with a full game played but the tiles flipped over
-- (so that letters are not visibile), use the scoring information to retrace
-- the game.

--------------------------------------------------------------------------------

-- the maximum hand length
maxHandLength = 7
-- the bonus for using the full hand
fullHandBonus = 50

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

--------------------------------------------------------------------------------

-- First move: must use at least 2 letters, must cross the middle square
-- (double the word).

-- In the list of points, we are given that this move scores 120
firstMoveScore :: Int
firstMoveScore = 120

-- The first word can be horizontal. In the given board the horizontal span of
-- tiles that crosses the center tile has 7 letters, only to the left. If we
-- use 0 to denote the center tile, then this is a span from -6 to 0.
firstMoveHorizontalStart, firstMoveHorizontalEnd :: Int
(firstMoveHorizontalStart, firstMoveHorizontalEnd) = (-6, 0)

-- Similarly, if the first word is vertical, using the vertical span that
-- crosses the middle tile we get these bounds.
firstMoveVerticalStart, firstMoveVerticalEnd :: Int
(firstMoveVerticalStart, firstMoveVerticalEnd) = (-4, 4)

-- Given the first move possibilities, we could hit a double letter at these
-- offsets.
doubleLetterIxs :: [Int]
doubleLetterIxs = [-4, 4]

-- A word is a pair: index of start and a list of tiles
type FirstWord = (Int, [Tile])

-- Scoring a word should be simple. Don't forget to double it at the end.
-- Also, don't forget the bonus if all 7 tiles have been used.
scoreFirstWord :: FirstWord -> Int
scoreFirstWord (ix, ls) = applyFullHandBonus . (*2) $ go 0 ix ls
  where
    -- only apply full hand bonus if full hand has been used
    applyFullHandBonus
      | length ls == maxHandLength = (+ fullHandBonus)
      | otherwise = id
    -- traverse each placed tile and see if we need to double it or not
    go score _ [] = score
    go score ix (l:ls)
      | ix `elem` doubleLetterIxs = go (score + 2 * letterScore) (ix + 1) ls
      | otherwise = go (score + letterScore) (ix + 1) ls
      where letterScore = scoreTable Map.! l

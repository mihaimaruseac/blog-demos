import Data.List
import Debug.Trace
import Text.Printf

-- number of tokens to use
numTokens = 6

-- Type alias for positions for more readable code
type Pos = (Int, Int)
-- Type alias for placements
type Board = [Pos]

-- All positions on the board
poss :: [Pos]
poss = [(i, j) | i <- [0..numTokens-1], j <- [0..numTokens-1]]

-- Compute distance (squared) between two positions
dist :: Pos -> Pos -> Int
dist (a, b) (c, d) = (c - a) ^ 2 + (d - b) ^ 2

-- Given a list of placements, return list of pairwise distances
dists :: Board -> [Int]
dists xs = [dist x x' | x <- xs, x' <- xs, x < x']

-- empty board
empty :: Board
empty = []

-- is done?
isDone :: Board -> Bool
isDone xs = length xs == numTokens

-- valid?
isValid :: Board -> Bool
isValid xs = nub ds == ds
  where
    ds = dists xs

-- the search
search :: Board -> [Board]
search board = do
  newBoard <- filter isValid $ map (:board) (poss \\ board)
  if isDone newBoard then [newBoard] else search newBoard

-- the main of the executable
main = mapM_ print $ search empty

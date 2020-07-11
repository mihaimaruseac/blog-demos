import Data.List
import Debug.Trace
import Text.Printf
import qualified Data.Map.Strict as Map

-- Size of grid
gridSize = 5

data Token = B | W deriving (Eq, Show)

-- A board is a mapping between position and token, together with count of
-- tokens already placed (in row by row fashion)
type Pos = (Int, Int)
type Board = (Int, Map.Map Pos Token)
type Square = [Pos]

-- Given 2 distinct points, construct the square they determine
squares :: Pos -> Pos -> [Square]
squares p0@(x0, y0) p1@(x1, y1) = [square1, square2]
  where
    dx = x1 - x0
    dy = y1 - y0
    square1 = [p0, p1, (x1 - dy, y1 + dx), (x0 - dy, y0 + dx)]
    square2 = [p0, p1, (x1 + dy, y1 - dx), (x0 + dy, y0 - dx)]

-- Valid points are those in the grid
validPoint :: Pos -> Bool
validPoint (x, y) = and [0 <= x, x < gridSize, 0 <= y, y < gridSize]

-- Valid squares have all corners in the grid
validSquare :: Square -> Bool
validSquare square = all validPoint square

-- Good squares have both tokens
badSquare :: Board -> Square -> Bool
badSquare (_, board) square = length (nub tokens) == 1
  where
    tokens = map (board Map.!) square

-- empty board
empty :: Board
empty = (0, Map.empty)

-- is done?
isDone :: Board -> Bool
isDone (c, _) = c == gridSize * gridSize

-- from token count to token position
tokenCountToPos :: Int -> Pos
tokenCountToPos c = (c `div` gridSize, c `mod` gridSize)

-- from token position to token count
tokenPosToCount :: Pos -> Int
tokenPosToCount (x, y) = x * gridSize + y

-- valid?
isValid :: Board -> Bool
isValid board@(c, _)
  | c <= 4 = True -- no squares to form yet
  | otherwise = null badSquares
  where
    _tr _ v = v --trace (printf "%s: %s" s (show v)) v
    p0 = _tr "p0" $ tokenCountToPos $ c - 1
    candidateSquarePoints = _tr "cSP" $ map tokenCountToPos [0..c-2]
    allSquares = _tr "aS" $ concatMap (squares p0) candidateSquarePoints
    validSquares = _tr "vS" $ filter validSquare allSquares
    placedSquare = all ((< c) . tokenPosToCount)
    placedSquares = _tr "pS" $ filter placedSquare validSquares
    badSquares = _tr "bS" $ filter (badSquare board) placedSquares

-- set ith token
setToken :: Board -> Token -> Board
setToken (c, board) token = (c + 1, Map.insert (tokenCountToPos c) token board)

-- the search
search :: Board -> [Board]
search board = do
  newBoard <- filter isValid $ map (setToken board) [B, W]
  if isDone newBoard then [newBoard] else search newBoard

-- given a board, check that the game is valid
validGame :: Board -> Bool
validGame (_, board) = abs (numW - numB) <= 1
  where
    allTokens = gridSize * gridSize
    allValues = map (board Map.!) $ map tokenCountToPos [0 .. allTokens - 1]
    numW = length $ filter (== W) allValues
    numB = allTokens - numW

-- the main of the executable
main = mapM_ print $ filter validGame $ search empty

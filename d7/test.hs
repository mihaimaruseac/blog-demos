import Control.Monad (guard)
import Data.List (nub)

n :: Int
n = 7

pts :: [(Int, Int)]
pts = [(i, j) | i <- [1..n], j <- [1..n]]

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

search :: Int -> [(Int, Int)] -> [[(Int, Int)]]
search k placed
  | k == n = [placed]
  | otherwise = do
    p <- pts
    guard $ not $ p `elem` placed
    let placed' = p:placed
    let dists = [dist x y | x <- placed', y <- placed', x < y]
    guard $ length (nub dists) == k * (k + 1) `div` 2
    search (k + 1) placed'

main :: IO ()
main = mapM_ print $ search 0 []

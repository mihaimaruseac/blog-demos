import Data.Maybe

-- If on day 1 we deposit x and on day 2 we deposit y, then the balance is:
--
--    Day    |  Balance
-- ----------------------
--      1    |   x
--      2    |   x +  y
--      3    |  2x +  y
--      4    |  3x + 2y
--      5    |  5x + 3y
--      ................
--      n    | F_n * x + F_{n-1} * y
--
--  where F_i is the ith Fibonacci number.
--
--  Thus, our problem is to find x and y to maximize n for which
--
--        F_n * x + F_{n-1} * y = 1000000
--
--  where x and y are nonnegative integers.

--  First, rather than peppering the code with 1000000, let's define it as a
--  constant.
target :: Int
target = 1000000

--  Since this is Haskell, we can create the infinite stream of Fibonacci
--  numbers and then use only what we need from it.
fib :: [Int]
fib = 1 : 1 : zipWith (+) fib (tail fib)

-- Since all numbers are positive, we only need the first few Fibonacci
-- numbers: only those smaller than `target`.
valid_fibs :: [Int]
valid_fibs = takeWhile (<= target) fib

-- We want the maximum n, so we want the maximum Fibbonacci coefficients.
-- Thus, we reverse the above list
candidates :: [Int]
candidates = reverse valid_fibs

-- The equation has two consecutive Fibbonacci coefficients, so we pair them.
pairs :: [(Int, Int)]
pairs = zip candidates (tail candidates)

-- Detemrine all solutions of F_n * x + F_{n-1} * y = t
-- Assumes that the second argument is a pair of consecutive Fibbonacci
-- numbers in decreasing order.
solve :: Int -> (Int, Int) -> [(Int, Int)]
solve t (a, b)
  | t < a  = []
  | t == a = [(1, 0)]
  | otherwise = catMaybes $ map find_y possible_xs
  where
    possible_xs = [0 .. (t `div` a)]
    find_y x
      | left `mod` b == 0 = Just (x, left `div` b)
      | otherwise = Nothing
      where
        left = t - a * x

-- Find al solutions that lead to target
allSols :: Int -> [(Int, Int)]
allSols t = concatMap (solve t) pairs

-- Display the solutions in a nice format, and have a main
main = mapM_ print $ allSols target

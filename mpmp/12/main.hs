import Data.List

-- Given a number, compute its prime decomposition
-- Eg: prime_decomp 42 = [3, 2, 2, 2]
prime_decomp :: Int -> [Int]
prime_decomp n = go n 2 []
  where
    go 1 _ l = l
    go n 2 l
      | even n = go (n `div` 2) 2 (2:l)
      | odd n = go n 3 l
    go n d l
      | n `mod` d == 0 = go (n `div` d) d (d:l)
      | otherwise = go n (d + 2) l

-- Given prime decom compute number of divisors
-- Eg: num_div_from_decomp [3, 2, 2, 2] = 8
num_div_from_decomp :: [Int] -> Int
num_div_from_decomp = product . map ((+1).length) . group

-- Given a number, compute number of divisors
num_divs :: Int -> Int
num_divs = num_div_from_decomp . prime_decomp

-- Search
desired_number_of_divisors = 64
search :: Int -> [Int]
search n
  | num_divs n == desired_number_of_divisors = n : search (n+1)
  | otherwise = search (n+1)

-- Main
main :: IO()
main = print $ head $ search 2

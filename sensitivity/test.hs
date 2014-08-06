--import Test.SmallCheck
import Test.QuickCheck

--prop :: Monad m => Int -> Int -> Int -> Property m
prop :: Int -> Int -> Int -> Property
prop x y a = and [x > 0, y > 0, y < x, a > 0] ==> maximum [abs (c - c'), abs (c - c'')] <= a'
  where
    x' = fromIntegral $ max x a
    y' = fromIntegral y
    c = y' / x'
    c' = y' / (x' + 1)
    c'' = (y' + 1) / (x' + 1)
    a' = 1 / (1 + fromIntegral a)

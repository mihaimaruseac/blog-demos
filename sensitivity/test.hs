import Control.Monad
import Data.List

import Test.SmallCheck
--import Test.QuickCheck

prop :: Monad m => Int -> Int -> Int -> Property m
--prop :: Int -> Int -> Int -> Property
prop x y a = and [x > 0, y > 0, y < x, a > 0] ==> maximum [abs (c - c'), abs (c - c'')] <= a'
  where
    x' = fromIntegral $ max x a
    y' = fromIntegral y
    c = y' / x'
    c' = y' / (x' + 1)
    c'' = (y' + 1) / (x' + 1)
    a' = 1 / (1 + fromIntegral a)

prop2 :: Monad m => Int -> Int -> Int -> Property m
--prop2 :: Int -> Int -> Int -> Property
prop2 x_ y_ a_ = x*y*a /= 0 ==> maximum [abs (q - q'), abs (q - q'')] <= a'
  where
    x = if x_ < 0 then -1000 * x_ else x_
    y = if 0 < y_ && y_ < x then y_ else x
    a = if a_ < 0 then -1000 * a_ else a_
    q = qual x y
    q' = qual (x+1) y
    q'' = qual (x+1) (y+1)
    a' = (10000*) $ exp (1 / fromIntegral (1 + a))
    qual x y = (10000 *) $ exp (fromIntegral y / fromIntegral (max x a))

prop3 :: Monad m => Int -> Int -> Int -> Property m
--prop2 :: Int -> Int -> Int -> Property
prop3 x_ y_ a_ = x*y*a /= 0 ==> maximum [abs (q - q'), abs (q - q'')] <= a'
  where
    x = if x_ < 0 then -1000 * x_ else x_
    y = if 0 < y_ && y_ < x then y_ else x
    a = if a_ < 0 then -1000 * a_ else a_
    q = qual x y
    q' = qual (x+1) y
    q'' = qual (x+1) (y+1)
    a' = (1 / fromIntegral (1 + a)) ^ 5
    qual x y = let c = (fromIntegral y / fromIntegral (max x a)) in c^5

expected a = liftM2 (/) sum genericLength [(fromIntegral y / fromIntegral (max x a)) ^ 1 / (1 / fromIntegral (1 + a)) ^ 1 |
  x <- [0..100], y <- [0..x]]

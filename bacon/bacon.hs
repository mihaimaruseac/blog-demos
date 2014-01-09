import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import System.Random

data D = A | B deriving Show

a = h . f . d . c . b
b c = ord c - i c - l
c 0 = [A]
c 1 = [B]
c n
  | odd n = B : c (n `div` 2)
  | otherwise = A : c (n `div` 2)
d l = l ++ replicate (g - e l) A
e = length
f = take g
g = l + l + l + l + l
h = reverse
i c = sum . map j . filter k $ [(c >)] <*> ['u', 'i']
j True = l
k = (False /=)
l = 1
m = "aeiou"
n = ['a' .. 'z']
o = n \\ m
p = e m - 1
q = e o - 1
r = randomRIO (0, p)
s = randomRIO (0, q)
t = concatMap
u (v:vs) (w:ws) (A:xs) = (m !! v) : u vs ws xs
u (v:vs) (w:ws) (B:xs) = (o !! w) : u vs ws xs
u _ _ [] = []
v x = g * x
z = do
  w <- getLine
  x <- replicateM (v $ e w) r
  y <- replicateM (v $ e w) s
  putStrLn . u x y . t a $ w
main = z

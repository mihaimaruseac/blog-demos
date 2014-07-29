{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import System.Random

import Debug.Trace

{- generic datatypes and functions -}

-- Random doesn't take into account the weights
class (Bounded a, Enum a, Ord a, Random a) => Item a where
  weight :: Num b => a -> b

type ExpR a = [(a, Double)]
type Exp a g = g -> Int -> ExpR a

expectedValues :: Item a => ExpR a
expectedValues = map (\i -> (i, weight i)) [minBound..]

{- testing -}

obtainedValues :: Item a => [a] -> ExpR a
obtainedValues = map (\is -> (head is, fromInteger . toInteger $ length is)) . group . sort

concatenateExpR :: Item a => ExpR a -> ExpR a -> ExpR a
concatenateExpR e1 e2 = map summer . groupBy item . sort $ e1 ++ e2
  where
    item (i, _) (i', _) = i == i'
    summer = foldl1 sum'
    sum' (i, c1) (_, c2) = (i, c1 + c2)

evalExpsR :: Item a => [ExpR a] -> ExpR a
evalExpsR = fix . map (\(i, c) -> (i, c / weight i)) . foldl1 concatenateExpR
  where
    fix i = let s = sum' i in map (\(x, j) -> (x, j / s)) i
    sum' = sqrt . sum . map ((**2) . snd)

testExpEC :: Item a => Exp a StdGen -> Int -> (Int, ExpR a)
testExpEC e c = (c, evalExpsR [e (mkStdGen g) c | g <- [1, 42, 52, 69, 88]])

testExpE :: Item a => (String, Exp a StdGen) -> (String, [(Int, ExpR a)])
testExpE (se, e) = (se, map (testExpEC e) [10, 100, 1000, 10000])

testExp :: Item a => a -> [(String, [(Int, ExpR a)])]
testExp i = snd (i, map testExpE
  [ ("basicExp", basicExp)
  --, ("basicWeighted", basicWeighted)
  , ("weightedRS", weightedRS)
  , ("priorityRS", priorityRS)
  ])

{- Experiments -}
-- non weighted
basicExp :: (RandomGen g, Item a) => Exp a g
basicExp g n = obtainedValues . take n $ randoms g

-- basic weighted
basicWeighted :: forall a g . (Item a, RandomGen g) => Exp a g
basicWeighted g n = obtainedValues . take n $ rs
  where
    expanded = concat . map (\i -> replicate (weight i) i) $ [minBound :: a ..]
    rs = map (expanded !!) $ randomRs (0, length expanded - 1) g

-- weighted RS
generalRS :: forall a g . (RandomGen g, Item a) => (Double -> Double -> Double) -> Exp a g
generalRS f g n = obtainedValues . map fst . take n . sortBy keySrt $ ks
  where
    is = [minBound :: a .. maxBound]
    us = filter (/= 0) $ randomRs (0, 1.0) g
    ks = zipWith (\i u -> (i, f (weight i) u)) is us
    keySrt (_, x) (_, y) = y `compare` x

weightedRS :: (Item a, RandomGen g) => Exp a g
weightedRS = generalRS (\w u -> u ** (1 / w))

priorityRS :: (Item a, RandomGen g) => Exp a g
priorityRS = generalRS (\w u -> w / u)

{- actual implementation -}

data Test = A | B | C | D deriving (Eq, Show, Ord, Enum, Bounded)

instance Random Test where
  random g = randomR (minBound, maxBound) g
  randomR (a, b) g = (toEnum x, g')
    where
      (x, g') = randomR (fromEnum a, fromEnum b) g

instance Item Test where
  weight A = 10
  weight B = 5
  weight C = 2
  weight D = 1

data Longer = L Integer deriving (Eq, Show, Ord)
gMAX = 100000

instance Bounded Longer where
  minBound = L 0
  maxBound = L gMAX

instance Enum Longer where
  fromEnum (L x) = fromInteger x
  toEnum x
    | x < 0 = L 0
    | x > gMAX = L gMAX
    | otherwise = L $ toInteger x

instance Random Longer where
  random g = randomR (L 0, L gMAX) g
  randomR (L a, L b) g = (L x, g')
    where
      (x, g') = randomR (a, b) g

instance Item Longer where
  weight (L a) = let x = fromInteger a in 10 * x * x + x

printE (en, er) = putStrLn en >> mapM_ printER er
printER (el, er) = putStrLn . concat $ ["\t", show el, "\t", show $ take 4 er]

main = do
  mapM_ printE $ testExp (undefined :: Test)
  putStrLn ""
  mapM_ printE $ testExp (undefined :: Longer)

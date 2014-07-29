{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import System.Random

{- generic datatypes and functions -}

-- Random doesn't take into account the weights
class (Bounded a, Enum a, Ord a, Random a) => Item a where
  weight :: Num b => a -> b

type ExpR a b = [(a, b)]
type Exp a b g = g -> Int -> ExpR a b

expectedValues :: (Item a, Num b) => ExpR a b
expectedValues = map (\i -> (i, weight i)) [minBound..]

obtainedValues :: (Item a, Num b) => [a] -> ExpR a b
obtainedValues = map (\is -> (head is, fromInteger . toInteger $ length is)) . group . sort

concatenateExpR :: (Item a, Ord b, Num b) => ExpR a b -> ExpR a b -> ExpR a b
concatenateExpR e1 e2 = map summer . groupBy item . sort $ e1 ++ e2
  where
    item (i, _) (i', _) = i == i'
    summer = foldl1 sum'
    sum' (i, c1) (_, c2) = (i, c1 + c2)

evalExpsR :: (Floating b, Ord b, Item a) => [ExpR a b] -> ExpR a b
evalExpsR = fix . map (\(i, c) -> (i, c / weight i)) . foldl1 concatenateExpR
  where
    fix i = let s = sum' i in map (\(x, j) -> (x, j / s)) i
    sum' = sqrt . sum . map ((**2) . snd)

-- non weighted
basicExp :: (RandomGen g, Item a, Num b) => Exp a b g
basicExp g n = obtainedValues . take n $ randoms g

-- basic weighted
basicWeighted :: forall a b g . (Item a, RandomGen g, Num b) => Exp a b g
basicWeighted g n = obtainedValues . take n $ rs
  where
    expanded :: Item a => [a]
    expanded = concat . map (\i -> replicate (weight i) i) $ [minBound..]
    rs :: Item a => [a]
    rs = map (expanded !!) $ randomRs (0, length expanded - 1) g

testExpEC :: (Floating b, Ord b, Item a) => Exp a b StdGen -> Int -> (Int, ExpR a b)
testExpEC e c = (c, evalExpsR [e (mkStdGen g) c | g <- [1, 42, 52, 69, 88]])

testExpE :: (Floating b, Ord b, Item a) => (String, Exp a b StdGen) -> (String, [(Int, ExpR a b)])
testExpE (se, e) = (se, map (testExpEC e) [10, 100, 1000, 10000])

testExp :: (Floating b, Ord b, Item a) => a -> [(String, [(Int, ExpR a b)])]
testExp i = snd (i, map testExpE
  [ ("basicExp", basicExp)
  , ("basicWeighted", basicWeighted)
  ])

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

main = mapM_ printE $ testExp (undefined :: Test)
printE (en, er) = putStrLn en >> mapM_ printER er
printER (el, er) = putStrLn . concat $ ["\t", show el, "\t", show er]

{-# LANGUAGE RecordWildCards #-}

import Control.Monad.State
import Data.List

{- globals -}
_z = 0.05 -- 0 <= _z <= 1
_cvi_g = 4 -- 0 < _cvi_g <= 5
_cvi_p = 2 -- 0 < _cvi_p <= 2
_val_p = 0.1
_val_g = 0.1
_xg = 0.3
_yg = 0.5
_xp = 0.2
_yp = 0.5
_nc = 3
runs = 40
initialg = 100
initialp = 100
minSize = 0

data Insect
  = Cricket
  | Parasite
  deriving (Show, Eq)

data Population = P
  { kind :: Insect -- type of population
  , ttl :: Age -- current time to live
  , ittl :: Age -- initial time to live
  , size :: Size -- size of population
  , id :: ID -- id of population
  , pid :: ID -- id of parent population (for graph representation)
  , nc :: Cycle -- cycle with no repro, for parasites (always 0 for crickets)
  }

instance Show Population where
  show p@(P{..}) = show id ++ "(" ++ show pid ++ ")" ++ show kind ++ "." ++ show size ++ " " ++ show ttl ++ "/" ++ show ittl

instance Eq Population where
  p1 == p2
    = kind p1 == kind p2
    && ttl p1 == ttl p2
    && ittl p1 == ittl p2
    && nc p1 == nc p2

type Age = Integer
type ID = Integer
type Size = Integer
type Cycle = Integer

data WorldState = WS
  { mid :: ID -- maximum ID found so far, next pop will have this id
  } deriving (Show, Eq)

type IW = State WorldState

-- no parent marker
noParent :: ID
noParent = -1

-- Initial state of the world.
initialState :: WorldState
initialState = WS 0

-- Returns the next available ID for a population.
getId :: IW ID
getId = do
  w@(WS {..}) <- get
  put $ w { mid = mid + 1 }
  return mid

-- Builds a new population
buildNew :: Insect -> Age -> Age -> Size -> ID -> IW Population
buildNew typ age iage size pid = do
  id <- getId
  return $ P typ age iage size id pid 0

-- Build initial population
buildInitial :: Size -> Size -> IW [Population]
buildInitial size_c size_p = do
  crickets <- buildNew Cricket _cvi_g _cvi_g size_c noParent
  parasites <- buildNew Parasite _cvi_p _cvi_p size_p noParent
  return [crickets, parasites]

-- Next world iteration.
nextIt :: [Population] -> IW [Population]
nextIt pops = do
  let ending_now = filter ((== 0) . ttl) pops
  let crickets_now = filter ((== Cricket) . kind) ending_now
  let parasites_now = filter ((== Parasite) . kind) ending_now
  let met = meeting ending_now
  living_again <- mapM descendPop $ pops \\ ending_now
  crickets <- mapM (cricketMutate met) crickets_now
  parasites <- mapM (parasiteMutate met) parasites_now
  let all = living_again ++ (concat crickets) ++ (concat parasites)
  return . filter existing $ all

-- Remove populations with no members
existing :: Population -> Bool
existing p@(P {..}) = size > minSize

-- Concatenate similar populations
concatP :: [Population] -> [Population]
concatP [] = []
concatP (p:ps)
  | p `elem` ps = undefined
  | otherwise = p : concatP ps

-- Returns if a subpopulation of Crickets met a subpopulation of Parasites.
meeting :: [Population] -> Bool
meeting p = any ((== Cricket) . kind) p && any ((== Parasite) . kind) p

-- Build a new population out of an elder one (living population, just older)
descendPop :: Population -> IW Population
descendPop p@(P {..}) = do
  p <- buildNew kind (ttl - 1) ittl size id
  return $ p { nc = nc }

-- Evolve a cricket population
cricketMutate :: Bool -> Population -> IW [Population]
cricketMutate False p@(P {..}) = do
  p <- buildNew kind ittl ittl (size *. (1 + _val_g)) id
  return [p]
cricketMutate True p@(P {..}) = do
  let living = size *. ((1 - _z) * (1 + _val_g))
  let decrease = if ittl > 1 then living *. _xg else 0
  let keep = living *. _yg
  let increase = living - decrease - keep
  pd <- buildNew kind (ittl - 1) (ittl - 1) decrease id
  pk <- buildNew kind ittl ittl keep id
  pi <- buildNew kind (ittl + 1) (ittl + 1) increase id
  return [pd, pk, pi]

-- Evolve a parasitic population
parasiteMutate :: Bool -> Population -> IW [Population]
parasiteMutate True p@(P {..}) = do
  p <- buildNew kind ittl ittl (size *. (1 + _val_p)) id
  return [p]
parasiteMutate False p@(P {..})
  | nc >= _nc = return []
  | otherwise = do
    let living = size
    let decrease = if ittl > 1 then living *. _xp else 0
    let keep = living *. _yp
    let increase = living - decrease - keep
    pd <- buildNew kind (ittl - 1) (ittl - 1) decrease id
    pk <- buildNew kind ittl ittl keep id
    pi <- buildNew kind (ittl + 1) (ittl + 1) increase id
    return [pd {nc = nc + 1}, pk {nc = nc + 1}, pi {nc = nc + 1}]

-- Iterations of the world
iterations :: [Population] -> IW [[Population]]
iterations p = do
  np <- nextIt p
  rest <- iterations np
  return $ np : rest

-- Aux function for multiply int with double.
(*.) :: Integer -> Double -> Integer
(*.) i d = toInteger . round $ fromIntegral i * d

-- Get a trace of the evolution
evolution :: Int -> Size -> Size -> [[Population]]
evolution size sizec sizep
  = take size $ evalState (buildInitial sizec sizep >>= iterations) initialState

-- Main
main :: IO ()
main = do
  print "Evolving..."
  let population = evolution runs initialg initialp
  dot $ concat population
  print "Making image.."

dotFile="cp.dot"

-- Print to the dot file
_d = appendFile dotFile

dot :: [Population] -> IO ()
dot p = do
  writeFile dotFile "digraph crickets {\n"
  _d "\tgraph [layout=dot rankdir=TD]\n"
  _d "\t 0 [shape=point]\n"
  _d "\t 1 [shape=point]\n"
  doDot p
  appendFile dotFile "}\n"

doDot :: [Population] -> IO ()
doDot [] = return ()
doDot (p@(P {..}) : ps) = do
  _d $ "\t" ++ show id ++ " " ++ "[shape="
  if kind == Cricket then _d "oval" else _d "box"
  _d $ ", label=" ++ show size
  if ttl == 0 then _d ", style=filled]\n" else _d "]\n"
  _d $ show pid ++ " -> " ++ show id ++ "\n"
  doDot ps

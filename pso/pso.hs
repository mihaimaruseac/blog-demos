{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Monad.State
import Data.List
import System.Random

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

type R = Double
type Position = (R, R)
type Velocity = (R, R)

data FName = Sphere | Rosenbrock | Rastrigin | Griewank deriving (Eq, Show)

data Function = F
  { name :: FName
  , f :: R -> R -> R -- the function
  , dom :: (R, R) -- domain
  , minima :: Position -- optimum point
  }

eval :: Function -> Position -> R
eval F{..} (x, y) = f x y

minVal :: Function -> R
minVal f = eval f $ minima f

instance Show Function where
  show f = show (name f)

-- Functions used
sphere = F Sphere f (-6, 6) (0, 0)
  where
    f x y = x * x + y * y

rosenbrok = F Rosenbrock f (-2.5, 2.5) (1, 1)
  where
    f x y = (1 - x) ** 2 + 100 * (y - x * x) ** 2

rastrigin = F Rastrigin f (-5.12, 5.12) (0, 0)
  where
    f x y = 20 + x * x + y * y - 10 * cos (2 * pi * x) - 10 * cos (2 * pi * y)

griewank = F Griewank f (-600, 600) (0, 0)
  where
    f x y = (x * x + y * y) / 4000 + 1 - cos x * cos (y / sqrt 2)

allFcts = [sphere, rosenbrok, rastrigin, griewank]

-- PSO

data TopologyType = Full | Ring | Quad deriving (Eq, Show, Enum)
data PSOType = Basic | IW | CF deriving (Eq, Show, Enum)
data Experiment = E
  { function :: Function
  , topologyType :: TopologyType
  , fi :: (R, R)
  , psoType :: PSOType
  } deriving (Show)

experiments = E <$> allFcts <*> [Full ..] <*> [(0.9, 0.1), (0.1, 0.9), (0.5, 0.5)] <*> [Basic ..]

type ID = Int
data Particle = P
  { position :: Position
  , v :: Velocity
  , pb :: Position
  } deriving (Show)

data WorldState = WS
  { randomg :: StdGen
  , particles :: V.Vector Particle
  , experiment :: Experiment
  } deriving (Show)

type IW = State WorldState

initialState :: StdGen -> Experiment -> WorldState
initialState g experiment = WS g V.empty experiment

initParticle :: (R, R) -> IW Particle
initParticle bounds@(x, y) = do
  s@WS{..} <- get
  let (x, gx) = randomR bounds randomg
  let (y, gy) = randomR bounds gx
  let vbounds = let m = abs x - y in (-m, m)
  let (vx, gvx) = randomR vbounds gy
  let (vy, gvy) = randomR vbounds gvx
  put $ s { randomg = gvy }
  return $ P (x, y) (vx, vy) (x, y)

initExperiment :: IW ()
initExperiment = do
  E{..} <- gets experiment
  ps <- replicateM popSize $ initParticle (dom function)
  s@WS{..} <- get
  put $ s { particles = V.fromList ps }

iterExperiment :: IW ()
iterExperiment = do
  E{..} <- gets experiment
  ps <- gets particles
  let lBest = getLocalBest topologyType function ps -- local best positions
  parts <- updateParticles psoType fi function ps lBest
  s <- get
  put $ s { particles = V.fromList parts }

updateParticles :: PSOType -> (R, R) -> Function -> V.Vector Particle -> V.Vector Position -> IW [Particle]
updateParticles psoType (fi1, fi2) f ps bs
  | V.null ps = return []
  | otherwise = do
    s@WS{..} <- get
    let (u1, g) = randomR (0, 1) randomg
    let (u2, g') = randomR (0, 1) g
    put $ s { randomg = g' }
    let P{..} = V.head ps
    let iw = if psoType == IW then iW else 1
    let cf = if psoType == CF then cF else 1
    let (vx, vy) = v
    let (px, py) = position
    let (bx, by) = pb
    let (lx, ly) = V.head bs
    let nvx = cf * (iw * vx + fi1 * u1 * (bx - px) + fi2 * u2 * (lx - px))
    let nvy = cf * (iw * vy + fi1 * u1 * (by - py) + fi2 * u2 * (ly - py))
    let npos@(nx, ny) = (px + nvx, py + nvy)
    let npb = if eval f npos < eval f pb then npos else pb
    result <- updateParticles psoType (fi1, fi2) f (V.tail ps) (V.tail bs)
    return $ P npos (nvx, nvy) npb : result

getLocalBest :: TopologyType -> Function -> V.Vector Particle -> V.Vector Position
getLocalBest Full f ps = V.map (const globalBest) ps
  where
    globalBest = fst . V.minimumBy posCompare . evalPos f $ ps
getLocalBest tt f ps = V.map (getBest tt) nvalues
  where
    values = evalPos f ps
    nvalues = V.zip nats values
    getBest tt (ix, pval) = fst . minimumBy posCompare . map (values V.!) $ neighs tt ix
    neighs Ring x = map (\x -> (popSize + x) `mod` popSize) [x - 1, x + 1]
    neighs Quad x = map (\x -> (popSize + x) `mod` popSize) [x - 1, x + 1, x - 4, x + 4]

evalPos :: Function -> V.Vector Particle -> V.Vector (Position, R)
evalPos f ps = V.map (evalParticlePos f) ps

evalParticlePos :: Function -> Particle -> (Position, R)
evalParticlePos f P{..} = (position, eval f position)

posCompare :: (Position, R) -> (Position, R) -> Ordering
posCompare (_, x) (_, y) = compare x y

-- main drivers
runExperiment :: IW (Position, R)
runExperiment = do
  initExperiment
  replicateM_ itMax iterExperiment
  WS{..} <- get
  let ps = V.map (\p -> p { position = pb p }) particles
  return $ V.minimumBy posCompare $ evalPos (function experiment) ps

doExperiment :: StdGen -> Experiment -> (Position, R)
doExperiment g e = evalState runExperiment $ initialState g e

doOneExperimentStep e 0 = return []
doOneExperimentStep e cnt = do
  res <- doOneExperimentStep e (cnt - 1)
  g <- randomIO
  let r = doExperiment (mkStdGen g) e
  putStrLn $ "IR " ++ show (expCount - cnt) ++ ": " ++ show r
  return $ r : res

doOneExperiment e = do
  putStrLn $ "Doing experiment " ++ show e
  res <- doOneExperimentStep e expCount
  putStrLn $ "Result: " ++ show (minimumBy posCompare res)

main = mapM doOneExperiment experiments

expCount = 10
popSize = 48
itMax = 1000
iW = 0.9
cF = 0.127
nats = V.fromList [0 .. popSize - 1]


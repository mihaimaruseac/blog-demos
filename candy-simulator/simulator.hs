{-# LANGUAGE RecordWildCards #-}

{-
 - Simulate the amount of action points (APs) one gets after consuming several
 - tons (the unit of measure) of candy (popular name for one commodity) in the
 - game of Pardus (http://pardus.at), based on the amount of sugar already in
 - his blood and the value of trip-control/meditation skill (TC).
 -}

import Control.Monad.State
import Data.List
import System.Random.TF
import System.Random.TF.Gen
import System.Random.TF.Instances

import Debug.Trace

-- number of hours of simulation
gInitialTime = 100000
-- min, max AP random gain
gMinAP = 200
gMaxAP = 250
-- diabetes factor
gFactor = 8
-- number of solutions to get
gNumSol = 50

data Action
  = Wait Int
  | Take Int
  deriving (Eq, Show)

plans :: [[Action]]
plans = concat
  [ [[Take x, Wait x] | x <- [1 .. 30]]
  ]

data Player a = P
  { g :: a
  , plan :: [Action]
  , planIndex :: Int
  , tripControl :: Int
  , timeLeft :: Int
  , sugarFactor :: Int
  , tonsTaken :: Int
  , apsGained :: Int
  }

instance Show (Player a) where
  show P{..} = intercalate " "
    [ show plan
    , "TC: " ++ show tripControl
    , "tons: " ++ show tonsTaken
    , "APs: " ++ show apsGained
    , "AP/t: " ++ (show $ fromIntegral apsGained / fromIntegral tonsTaken)
    , "AP/h: " ++ (show $ fromIntegral apsGained / fromIntegral gInitialTime)
    ] ++ "\n"

makeAgent :: Int -> [Action] -> g -> Player g
makeAgent tc p g = P g p 0 tc gInitialTime 0 0 0

stepAgent :: RandomGen g => State (Player g) ()
stepAgent = do
  p <- get
  if timeLeft p < 0 then return () else (runPlanStep >> stepAgent)

runPlanStep :: RandomGen g => State (Player g) ()
runPlanStep = do
  p@P{..} <- get
  case plan !! planIndex of
    Wait t -> put $ p
      { timeLeft = timeLeft - t
      , planIndex = (planIndex + 1) `mod` (length plan)
      , sugarFactor = max 0 (sugarFactor - t)
      }
    Take t -> do
      let (ap, g') = generateAPs tripControl sugarFactor t g
      put $ p
        { planIndex = (planIndex + 1) `mod` (length plan)
        , sugarFactor = sugarFactor + t
        , tonsTaken = tonsTaken + t
        , apsGained = apsGained + ap - 10
        , g = g'
        }

generateAPs :: RandomGen g => Int -> Int -> Int -> g -> (Int, g)
generateAPs tc sg cd g = (max 0 $ appt * cd - gFactor * s, g')
  where
    (appt, g') = randomR (gMinAP, gMaxAP) g
    s = sum . map (max 0) . map (\x -> x - tc) $ [sg .. sg + cd - 1]

evalPlans :: RandomGen g => g -> Int -> [Player g]
evalPlans g tc = map (execState stepAgent) $ zipWith (makeAgent tc) plans gs
  where
    gs = map (splitn (level g) pbits) [0 .. fiPlan plans - 1]
    pbits = ceiling (log (fiPlan plans) / log 2)
    fiPlan p = fromIntegral . length $ p

topKAPTPlans, topKAPHPlans :: [Player g] -> [Player g]
topKAPHPlans = topKPlans apsPerHour gNumSol
topKAPTPlans = topKPlans apsPerTon gNumSol

topKPlans :: (Player g -> Player g -> Ordering) -> Int -> [Player g] -> [Player g]
topKPlans cmp k plans = take k . sortBy cmp $ plans

apsPerTon, apsPerHour :: (Player g -> Player g -> Ordering)
apsPerTon p1 p2 = getAPPerTon p2 `compare` getAPPerTon p1
apsPerHour p1 p2 = getAPPerHour p2 `compare` getAPPerHour p1

getAPPerTon, getAPPerHour :: Player g -> Double
getAPPerTon P{..} = fromIntegral apsGained / fromIntegral tonsTaken
getAPPerHour P{..} = fromIntegral apsGained / fromIntegral gInitialTime

getSolutionForTC :: RandomGen g => g -> Int -> ([Player g], [Player g])
getSolutionForTC g tc = (topKAPTPlans evaled, topKAPHPlans evaled)
  where
    evaled = evalPlans g tc

generateSolution :: Int -> IO ()
generateSolution tc = do
  t <- mkSeedUnix
  putStrLn $ "Generating solutions for TC=" ++ show tc
  let (bestAPT, bestAPH) = getSolutionForTC (seedTFGen t) tc
  putStrLn "Best solution in terms of AP/t:"
  print bestAPT
  putStrLn ""
  putStrLn "Best solution in terms of AP/h:"
  print bestAPH
  putStrLn "==================\n"

main :: IO ()
main = mapM_ generateSolution [0..5]

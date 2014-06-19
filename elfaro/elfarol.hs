{-# LANGUAGE RecordWildCards #-}

import Control.Monad.State
import System.Random

import Debug.Trace

type Crowd = Int
type History = [Crowd]

data Strategy
  = Mirror
  | Average
  | Trend
  | Same1
  | Same2
  | Same5
  deriving (Eq, Enum, Bounded, Show)

instance Random Strategy where
  randomR (min, max) g = (a1, g1)
    where
      (a, g1) = randomR (fromEnum min, fromEnum max) g
      a1 = toEnum a
  random = randomR (minBound, maxBound)

type Irishmen = [Strategy]
type IrishHist = [Irishmen]

data WorldState = WS
  { randomg :: StdGen
  , history :: History
  , irishmen :: IrishHist
  } deriving (Show)

type IW = State WorldState

-- Initial history
initial :: History
initial = [44, 78, 56, 15, 23, 67, 84, 34, 45, 76, 40, 56, 22, 35]

-- Strategy predictions
predict :: Strategy -> History -> Crowd
predict Mirror = (50 -) . head
predict Average = average
predict Trend = trend
predict Same1 = head
predict Same2 = head . drop 1
predict Same5 = head . drop 4

average :: History -> Crowd
average h@(x1:x2:x3:x4:_) = (x1 + x2 + x3 + x4) `div` 4

trend :: History -> Crowd
trend h
  | tr > 100 = 100
  | tr < 0 = 0
  | otherwise = tr
  where
    a = head h
    b = head . drop 7 $ h
    alpha = (b - a) `div` 7
    tr = a - alpha

-- Initial state
initialState :: StdGen -> WorldState
initialState g = WS g initial []

-- Evolution
evolve :: IW ()
evolve = do
  strategies <- replicateM 100 randomStrategy
  h <- gets history
  let expected = length . filter (<= 60) . map (flip predict h) $ strategies
  s@(WS {..}) <- get
  put $ s { irishmen = strategies : irishmen, history = expected : h }

-- Get random strategy for one irishman.
randomStrategy :: IW Strategy
randomStrategy = do
  st@(WS {..}) <- get
  let (s, g) = random randomg
  put $ st { randomg = g }
  return s

-- Run the entire simulation
simulation :: StdGen -> WorldState
simulation g = execState (replicateM 100 evolve) (initialState g)

avg :: (Integral a) => [a] -> Double
avg l = (fromIntegral (sum l)) / (fromIntegral (length l))

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

between :: Int -> Int -> Int -> Bool
between a b x = a < x && x <= b

-- Main.
main = do
  g <- getStdGen
  let t@(WS {..}) = simulation g
  let h = take 100 history
  putStrLn $ "Media prezentelor: " ++ show (avg h)
  putStrLn $ "Prezenta [ 0, 50]: " ++ show (count (<= 50) h)
  putStrLn $ "Prezenta (50, 60]: " ++ show (count (between 50 60) h)
  putStrLn $ "Prezenta (60, 70]: " ++ show (count (between 60 70) h)
  putStrLn $ "Prezenta (70,100]: " ++ show (count (> 70) h)
  writeFile "elfaro.dot" "graph elfaro {\n"
  _d "\tgraph [layout=dot rankdir=TD]\n"
  _d "\tgraph [layout=dot rankdir=TD]\n"
  _d "\tnode [shape=box, style=filled]\n"
  dot 0 h irishmen
  _d "}\n"

-- Print to the dot file
_d = appendFile "elfaro.dot"

dot :: Int -> History -> IrishHist -> IO ()
dot x (h:hs) (i:is) = do
  _d $ "\tp" ++ show x ++ " [label=" ++ show h ++ ", fillcolor=" ++ popcolor h ++ "]\n"
  let y = length . filter (== Mirror) $ i
  _d $ "\tp" ++ show x ++ "M [label=" ++ show y ++ ", fillcolor=" ++ popcolor' y ++ "]\n"
  _d $ "\tp" ++ show x ++ " -- p" ++ show x ++ "M\n"
  let y = length . filter (== Average) $ i
  _d $ "\tp" ++ show x ++ "A [label=" ++ show y ++ ", fillcolor=" ++ popcolor' y ++ "]\n"
  _d $ "\tp" ++ show x ++ "M -- p" ++ show x ++ "A\n"
  let y = length . filter (== Trend) $ i
  _d $ "\tp" ++ show x ++ "T [label=" ++ show y ++ ", fillcolor=" ++ popcolor' y ++ "]\n"
  _d $ "\tp" ++ show x ++ "A -- p" ++ show x ++ "T\n"
  let y = length . filter (== Same1) $ i
  _d $ "\tp" ++ show x ++ "s1 [label=" ++ show y ++ ", fillcolor=" ++ popcolor' y ++ "]\n"
  _d $ "\tp" ++ show x ++ "T -- p" ++ show x ++ "s1\n"
  let y = length . filter (== Same2) $ i
  _d $ "\tp" ++ show x ++ "s2 [label=" ++ show y ++ ", fillcolor=" ++ popcolor' y ++ "]\n"
  _d $ "\tp" ++ show x ++ "s1 -- p" ++ show x ++ "s2\n"
  let y = length . filter (== Same5) $ i
  _d $ "\tp" ++ show x ++ "s5 [label=" ++ show y ++ ", fillcolor=" ++ popcolor' y ++ "]\n"
  _d $ "\tp" ++ show x ++ "s2 -- p" ++ show x ++ "s5\n"
  dot (x + 1) hs is
dot _ _ _ = return ()

popcolor :: Crowd -> String
popcolor x
  | x < 50 = "blue"
  | x < 60 = "green"
  | x < 70 = "orange"
  | x <= 100 = "red"

popcolor' :: Crowd -> String
popcolor' x
  | x < 10 = "blue"
  | x < 20 = "cyan"
  | x < 30 = "azure"
  | x < 40 = "yellow"
  | x < 50 = "green"
  | x < 60 = "magenta"
  | x < 70 = "purple"
  | x < 80 = "orangered"
  | x < 90 = "red"
  | x <= 100 = "darkred"

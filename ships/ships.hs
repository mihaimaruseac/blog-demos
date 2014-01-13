import System.Environment(getArgs)

type Speed = Float
type Dir = (Float, Float)
type Point = (Float, Float)
type TimeStamp = Float
type TimeIncrement = Float
type Distance = Float

data Ship = Ship {
  position :: Point,
  direction :: Dir,
  speed :: Speed
  } deriving (Show)

data World = World {
  firstShip :: Ship,
  secondShip :: Ship,
  timeStamp :: TimeStamp
  } deriving (Show)

dist :: Point -> Point -> Distance
dist (x,y) (x', y') = sqrt ((x - x') ^ 2 + (y - y') ^ 2)

slope :: Point -> Point -> Dir
slope p1@(x,y) p2@(x', y') = ((x'- x)/d, (y'-y)/d)
  where
    d = dist p1 p2

distShip :: World -> Distance
distShip w = dist (position . firstShip $ w) (position . secondShip $ w)

getNewPos :: Ship -> TimeIncrement -> Point
getNewPos (Ship (x,y) (z,u) s) t = (x+z*s*t/w, y+u*s*t/w)
  where
    w = sqrt $ z ** 2 + u ** 2

advanceWorld :: World -> TimeIncrement -> World
advanceWorld w@(World s1 s2 t) dt = w { firstShip = s1', 
  secondShip = s2', timeStamp = t + dt}
  where
    p2 = getNewPos s2 dt
    p1 = getNewPos s1 dt
    s1' = s1 { position = p1}
    s2' = s2 { position = p2, direction = slope p2 p1}

createWorld :: Distance -> Speed -> World
createWorld initialDistance sp = World (Ship (42, 24) (-5.42, -2.34) sp) 
  (Ship (initialDistance, 0) (-1, 0) sp) 0

iterateUntil :: (a -> a) -> (a -> Bool) -> a -> [a]
iterateUntil f p e = go [e]
  where
    go l@(x:xs)
      | p x = l
      | otherwise = go ((f x):l)

doEvolution :: World -> TimeIncrement -> TimeStamp -> [World]
doEvolution w t tm = iterateUntil (flip advanceWorld t) (\x -> timeStamp x > tm) w

printResults w = do
  appendFile "dist" $ show $ timeStamp w 
  appendFile "dist" "\t"
  appendFile "dist" $ show $ distShip w
  appendFile "dist" "\n"
  appendFile "trace" $ show $ fst $ position $ firstShip w
  appendFile "trace" "\t"
  appendFile "trace" $ show $ snd $ position $ firstShip w
  appendFile "trace" "\t"
  appendFile "trace" $ show $ fst $ position $ secondShip w
  appendFile "trace" "\t"
  appendFile "trace" $ show $ snd $ position $ secondShip w
  appendFile "trace" "\n"

main = do
  args <- getArgs
  let d = read $ args !! 0
  let t = read $ args !! 1
  let s = read $ args !! 2
  let w = createWorld d s
  let maxt = read $ args !! 3
  writeFile "dist" ""
  writeFile "trace" ""
  mapM_ printResults $ doEvolution w t maxt

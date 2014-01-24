import System.Environment(getArgs)

type Speed = Float
type Dir = (Float, Float)
type Point = (Float, Float)
type TimeStamp = Float
type TimeIncrement = Float
type Distance = Float

data Actor = Actor {
  position :: Point,
  direction :: Dir,
  speed :: Speed
  } deriving (Show)

data World = World {
  man :: Actor,
  injuredRaptor :: Actor,
  firstRaptor :: Actor,
  secondRaptor :: Actor,
  timeStamp :: TimeStamp
  } deriving (Show)

dist :: Point -> Point -> Distance
dist (x,y) (x', y') = sqrt ((x - x') ^ 2 + (y - y') ^ 2)

slope :: Point -> Point -> Dir
slope p1@(x,y) p2@(x', y') = ((x'- x)/d, (y'-y)/d)
  where
    d = dist p1 p2

getNewPos :: Actor -> TimeIncrement -> Point
getNewPos (Actor (x,y) (z,u) s) t = (x+z*s*t/w, y+u*s*t/w)
  where
    w = sqrt $ z ** 2 + u ** 2

advanceWorld :: World -> TimeIncrement -> World
advanceWorld w@(World m i f s t) dt = w { man = m', injuredRaptor = i',
  firstRaptor = f', secondRaptor = s', timeStamp = t + dt}
  where
    pm = getNewPos m dt
    pi = getNewPos i dt
    pf = getNewPos f dt
    ps = getNewPos s dt
    m' = m { position = pm }
    i' = i { position = pi, direction = slope pi pm }
    f' = f { position = pf, direction = slope pf pm }
    s' = s { position = ps, direction = slope ps pm }

createWorld :: Dir -> Speed -> Distance -> Speed -> Speed -> World
createWorld mandir sm d si sr = 
  World (Actor (0, 0) mandir sm)
        (Actor (0, d*sq3/3) (0, -1) si)
        (Actor (-0.5*d, -d*sq3/6) (0.5*sq3, 0.5) sr)
        (Actor (0.5*d, -d*sq3/6) (-0.5*sq3, 0.5) sr)
	0
  where
    sq3 = sqrt 3

iterateUntil :: (a -> a) -> (a -> Bool) -> a -> [a]
iterateUntil f p e = go [e]
  where
    go l@(x:xs)
      | p x = l
      | otherwise = go ((f x):l)


doEvolution :: World -> TimeIncrement -> Distance -> [World]
doEvolution w t dm = iterateUntil (flip advanceWorld t) f w
  where
    f world = or [d injuredRaptor, d firstRaptor, d secondRaptor, max]
      where
	m = man world
	d raptor = (dist (position m) (position $ raptor world)) < dm
	max = timeStamp world > 10000

printResults w = do
  appendFile "trace" $ show $ fst $ position $ man w
  appendFile "trace" "\t"
  appendFile "trace" $ show $ snd $ position $ man w
  appendFile "trace" "\t"
  appendFile "trace" $ show $ fst $ position $ injuredRaptor w
  appendFile "trace" "\t"
  appendFile "trace" $ show $ snd $ position $ injuredRaptor w
  appendFile "trace" "\t"
  appendFile "trace" $ show $ fst $ position $ firstRaptor w
  appendFile "trace" "\t"
  appendFile "trace" $ show $ snd $ position $ firstRaptor w
  appendFile "trace" "\t"
  appendFile "trace" $ show $ fst $ position $ secondRaptor w
  appendFile "trace" "\t"
  appendFile "trace" $ show $ snd $ position $ secondRaptor w
  appendFile "trace" "\n"

main = do
  args <- getArgs
  let d = read $ args !! 0
  let ms = read $ args !! 1
  let rs = read $ args !! 2
  let is = read $ args !! 3
  let angle = (read $ args !! 4) * pi / 180
  let maxd = read $ args !! 5
  let t = read $ args !! 6
  let w = createWorld (cos angle, sin angle) ms d is rs
  writeFile "trace" ""
  mapM_ printResults $ doEvolution w t maxd


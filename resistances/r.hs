import Data.Maybe (fromJust)

type Resistance = Double
type Conductance = Double

type Network a = [(a, Node a)]
type Node a = [WireTo a]
type WireTo a = (a, Resistance)

(===) :: (Eq a) => (a, b) -> (a, b) -> Bool
(a, _) === (b, _) = a == b

(=/=) :: (Eq a) => (a, b) -> (a, b) -> Bool
(=/=) a = not . (a ===)

{-
Starts the solving phase testing if each node is defined.
-}
solve :: (Ord a) => Network a -> a -> a -> Resistance
solve n st en
  | stn == Nothing = error "Wrong start node"
  | enn == Nothing = error "Wrong end node"
  | otherwise = solve' n st en
  where
    stn = lookup st n
    stnd = fromJust stn
    enn = lookup en n

solve' :: (Ord a) => Network a -> a -> a -> Resistance
solve' n st en
  | null candidates = getSolution n st en
  | otherwise = solve' (removeNode n (head candidates)) st en
  where
    candidates = filter (\(x,_) -> x /= st && x /= en) n

removeNode :: (Ord a) => Network a -> (a, Node a) -> Network a
removeNode net w@(tag, nod)
  | length nod == 1 = filter (=/= w) net
  | otherwise = filtered `joinParts` keep `joinParts` new
  where
    affectedTags = map fst nod
    -- construct the unaffected nodes list
    unaffectedNodes = filter (\(a,_) -> a `notElem` affectedTags) net
    keep = filter (=/= w) unaffectedNodes
    -- and the affected ones
    change = filter (\(a,_)-> a `elem` affectedTags) net
    -- remove the node from all the maps
    filtered = map (purify tag) change
    -- compute the sum of inverses
    sumR = sum . map ((1/) . snd) $ nod
    pairs = [(x, y) | x <- affectedTags, y <- affectedTags, x < y]
    new = foldl joinParts [] . map (buildFromTags nod sumR) $ pairs

purify :: (Eq a) => a -> (a, Node a) -> (a, Node a)
purify t (tag, wires) = (tag, filter (\(a,_)->a /= t) wires)

buildFromTags :: (Ord a) => Node a -> Conductance -> (a, a) -> Network a
buildFromTags n s (x, y) = buildPart x y (s * xx * yy)
  where
    xx = fromJust . lookup x $ n
    yy = fromJust . lookup y $ n

getSolution :: (Eq a) => Network a -> a -> a -> Resistance
getSolution n st en = fromJust . lookup en . fromJust . lookup st $ n

-- builds a simple network: a simple wire
buildPart :: (Ord a) => a -> a -> Resistance -> Network a
buildPart a b r
  | r < 0 = error "Negative resistance is not allowed"
  | a == b = error "No wire can have the same ends"
  | a > b = buildPart b a r
  | otherwise = [(a, [(b, r)]), (b, [(a, r)])]

-- joins two networks
joinParts :: (Eq a) => Network a -> Network a -> Network a
joinParts [] ns = ns
joinParts (n:ns) nodes
  | other == Nothing = n : joinParts ns nodes
  | otherwise = (fst n, snd n `combineNodes` m) : joinParts ns nodes'
  where
    other = lookup (fst n) nodes
    m = fromJust other
    nodes' = filter (=/= n) nodes

{-
Combines two nodes (wires leaving the same node, declared in two parts of
network.
-}
combineNodes :: (Eq a) => Node a -> Node a -> Node a
combineNodes = foldr (\(i,r) n -> addWireTo n i r)

{-
Adds a wire to a node, doing the right thing if there is a wire there already.
-}
addWireTo :: (Eq a) => Node a -> a -> Resistance -> Node a
addWireTo w a r = parallel $ (a,r) : w

{-
Reduce a list of wires by reducing parallel resistors to a single one.
-}
parallel :: (Eq a) => [WireTo a] -> [WireTo a]
parallel [] = []
parallel (w:ws) = w' : parallel ws'
  where
    ws' = filter (=/= w) ws
    ws'' = w : filter (=== w) ws
    w' = if ws'' == [] then w else foldl1 parallel' ws''

{-
Reduce two wires to a single one, if they form parallel resistors.
-}
parallel' :: (Eq a) => WireTo a -> WireTo a -> WireTo a
parallel' (a, r1) (b, r2)
  | a == b = (a, r1 * r2 / (r1 + r2))
  | otherwise = error "Cannot reduce: not parallel resistors"

{-
Simple test: a network with only a wire.
-}
testSimple :: Network Int
testSimple = buildPart 0 1 5

{-
Second test: a network with two parallel wires.
-}
testSimple' :: Network Int
testSimple' = buildPart 0 1 10 `joinParts` buildPart 0 1 6

{-
Third test: a network with two series resistors.
-}
testSimple'' = buildPart 0 1 40 `joinParts` buildPart 1 2 2

{-
Fourth test: tetrahedron
-}
testTetra :: Network Int
testTetra
  = buildPart 0 1 1 `joinParts`
    buildPart 0 2 1 `joinParts`
    buildPart 1 2 1 `joinParts`
    buildPart 0 3 1 `joinParts`
    buildPart 1 3 1 `joinParts`
    buildPart 2 3 1

{-
Fifth test: 2 squares
-}
testSquares :: Network (Int, Int)
testSquares = foldl joinParts [] . map (\(x, y) -> buildPart x y 1) $ list
  where
    list = [((0, 0), (0, 1)), ((0, 1), (0, 2)), ((0, 2), (1, 2)),
      ((1, 2), (1, 1)), ((1, 1), (0, 1)), ((1, 1), (1, 0)), ((0, 0), (1, 0))]

build :: Int -> Network (Int, Int)
build n = foldl joinParts [] . map (\(x, y) -> buildPart x y 1) $ list
  where
    list = [(x, y) | x <- points, y <- points, x `neigh` y, x < y]
    points = fillBelow n

fillBelow :: Int -> [(Int, Int)]
fillBelow n = [(x, y) | x <- [-n .. n], y <- [-n ..n]]

neigh :: (Int, Int) -> (Int, Int) -> Bool
neigh (a, b) (c, d) = abs (a - c) + abs (b - d) == 1

solveXKCD :: Int -> Resistance
solveXKCD n = solve (build n) (0, 0) (1, 2)

main = mapM (print . \x -> (x, solveXKCD x)) [3..]


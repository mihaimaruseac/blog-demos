import System.Environment (getArgs)
import Text.Printf

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Debug.Trace

type WordDict = Map.Map Int (Set.Set String)

main = do
  wordlist:_ <- getArgs
  printf "Got wordlist at %s\n" wordlist
  ls <- lines <$> readFile wordlist
  printf "Got %d words\n" $ length ls
  let md = buildMapData ls
  printf "Build a map data of %d elements\n" $ Map.size md
  let maxLength = fst $ Map.findMax md
  printf "Maximal element %d\n" maxLength
  mapM_ print $ search md maxLength

buildMapData :: [String] -> WordDict
buildMapData = Map.fromListWith Set.union . map pair
  where
    pair w = (length w, Set.singleton w)

search :: WordDict -> Int -> [[String]]
--search wd l = trace (printf "Searching words of length %d" l) $ search' wd l
search wd l
  | l < 1 = []
  | Set.null candidates = search wd $ l - 1
  | null searchResults = search wd $ l - 1
  | otherwise = searchResults
  where
    candidates = Map.findWithDefault Set.empty l wd
    searchResults = filter (not . null) . map (searchWord wd) $ Set.toList candidates

-- assume w already in wd
searchWord :: WordDict -> String -> [String]
--searchWord wd w = trace (printf "Looking for %s" w) $ searchWord' wd w
searchWord wd w
  | null pre = [w] -- singleton
  | pre `Set.member` less = searchAndPrepend wd w pre
  | post `Set.member` less = searchAndPrepend wd w post
  | otherwise = [] -- dead end
  where
    pre = init w
    post = tail w
    less = Map.findWithDefault Set.empty (length w - 1) wd

searchAndPrepend :: WordDict -> String -> String -> [String]
--searchAndPrepend wd now prev = trace (printf "Found link %s - %s" now prev) $ searchAndPrepend' wd now prev
searchAndPrepend wd now prev = case searchWord wd prev of
  [] -> []
  list -> now : list

testWordDict :: WordDict
testWordDict = Map.fromList
  [ (1, Set.fromList ["a", "i"])
  , (2, Set.fromList ["aa", "an", "ad", "ah", "am", "as", "at", "be", "in", "id", "me", "we"])
  , (3, Set.fromList ["aaa", "and", "ada", "nah", "cam", "dam"])--, "bee", "win"])
  , (4, Set.fromList ["wind", "wine", "beer"])
  , (6, Set.fromList ["rewind"])
  ]

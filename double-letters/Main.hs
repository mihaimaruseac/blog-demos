import System.Environment (getArgs)
import Text.Printf

main = do
  wordlist:_ <- getArgs
  printf "Got wordlist at %s\n" wordlist
  -- map init to remove the ending '\n'
  ls <- map init . lines <$> readFile wordlist
  printf "Got %d words\n" $ length ls
  let words_with_2_letters = filterWordsOfLength 2 ls
      words_with_7_letters = filterWordsOfLength 7 ls
  printf "Got %d words of length 2 and %d words of length 7\n" (length words_with_2_letters) (length words_with_7_letters)
  let solution = filter (\w -> hasValidSubwords (permute w) words_with_2_letters) words_with_7_letters
  printf "Got %d words in solution:\n" $ length solution
  mapM_ (displaySolutionWord words_with_2_letters) $ solution

filterWordsOfLength :: Int -> [String] -> [String]
filterWordsOfLength l = filter (\w -> length w == l)

hasValidSubwords :: String -> [String] -> Bool
hasValidSubwords "" _ = True
hasValidSubwords [_] _ = True
hasValidSubwords (x:xs@(y:_)) ws
  | [x, y] `elem` ws = hasValidSubwords xs ws
  | otherwise = False

permute :: String -> String
permute [a,b,f,e,g,d,c] = [a,b,c,d,e,f,g]
permute _ = error "Only works on 7 letter words"

displaySolutionWord :: [String] -> String -> IO ()
displaySolutionWord ws w = printf "\t%s %s %s\n" w (show wsw) (show wsf)
  where
    [a,b,f,e,g,d,c] = w
    wsw = [[a,b], [b,c], [c,d], [d,e], [e,f], [f,g]]
    wsf = map (`elem` ws) wsw

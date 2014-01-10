import Control.Monad.State

type Output = String
type Offset = Int

data Command
  = IP -- >
  | DP -- <
  | IB -- +
  | DB -- -
  | O  -- .
  | J Offset -- [ and ]
  deriving (Eq, Show)

type Program = [Command]

parseDummy :: String -> Program
parseDummy = parseDummy' [] 0

parseDummy' :: [Offset] -> Offset -> String -> Program
parseDummy' [] _ "" = []
parseDummy' ps p ('>':ss) = IP : parseDummy' ps (p + 1) ss
parseDummy' ps p ('<':ss) = DP : parseDummy' ps (p + 1) ss
parseDummy' ps p ('+':ss) = IB : parseDummy' ps (p + 1) ss
parseDummy' ps p ('-':ss) = DB : parseDummy' ps (p + 1) ss
parseDummy' ps p ('.':ss) = O : parseDummy' ps (p + 1) ss
parseDummy' ps p ('[':ss) = parseDummy' (p:ps) p ss
parseDummy' (p':ps) p (']':ss) = J p' : parseDummy' ps p ss
parseDummy' _ _ _ = error "Invalid program"

data ParsingState = PS
  { pos :: Offset   -- current position
  , ops :: [Offset] -- list of openings
  , src :: String    -- source
  }

parse :: String -> Program
parse = evalState parse' . PS 0 []

parse' :: State ParsingState Program
parse' = do
  s <- gets src
  if s == "" then finish else doParse s

finish :: State ParsingState Program
finish = do
  o <- gets ops
  if o == [] then return [] else fail "Invalid program"

doParse :: String -> State ParsingState Program
doParse s = do
  modify (\s' -> s' {src = tail s})
  case head s of
    '>' -> parseCmd IP
    '<' -> parseCmd DP
    '+' -> parseCmd IB
    '-' -> parseCmd DB
    '.' -> parseCmd O
    '[' -> saveJumpPoint
    ']' -> saveJumpCmd
    _ -> fail "Invalid program"

parseCmd :: Command -> State ParsingState Program
parseCmd i = do
  modify (\s -> s {pos = pos s + 1})
  is <- parse'
  return $ i : is

saveJumpPoint :: State ParsingState Program
saveJumpPoint = do
  modify (\s -> s {ops = pos s : ops s})
  parse'

saveJumpCmd :: State ParsingState Program
saveJumpCmd = do
  os <- gets ops
  if os == [] then fail "Invalid program" else doSaveJumpCmd os

doSaveJumpCmd ops = do
  modify (\s -> s {ops = tail ops})
  is <- parse'
  return $ J (head ops) : is


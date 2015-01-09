{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (put, get)
import Data.Acid
import Data.IxSet
import Data.SafeCopy
import Data.Typeable
import System.Clock
import System.Console.CmdArgs

import qualified System.Console.CmdArgs.Explicit as CA

data Details = Details
  { stringVal :: String
  , intVals :: [Int]
  } deriving (Eq, Ord, Show, Typeable)

$(deriveSafeCopy 0 'base ''Details)

instance Indexable Details where
  empty = ixSet
    [ ixFun $ \dt -> [stringVal dt]
    , ixFun $ \dt -> intVals dt
    ]

data Test = Test
  { nextID :: Int
  , dtls :: IxSet Details
  } deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Test)

initialState :: Test
initialState = Test 0 empty

cleanTest :: Update Test ()
cleanTest = put initialState

queryTest :: Query Test Test
queryTest = ask

searchTest :: String -> Query Test (Maybe Details)
searchTest k = getOne . (@= k) . dtls <$> ask

sumTest :: Query Test Int
sumTest = sum . map (sum . intVals) . toList . dtls <$> ask

sizeTest :: Query Test Int
sizeTest = size . dtls <$> ask

insertTest :: Int -> Update Test ()
insertTest x = do
  Test{..} <- get
  let new = Details (concat [show nextID, "-", show x]) [x]
  put $ Test
    { nextID = succ nextID
    , dtls = insert new dtls
    }

$(makeAcidic ''Test ['cleanTest, 'queryTest, 'sizeTest, 'sumTest, 'insertTest, 'searchTest])

main :: IO ()
main = do
  args' <- cmdArgs testArgs
  case args' of
    Help -> showHelp
    _ -> mainDB args'

showHelp :: IO ()
showHelp = print $ CA.helpText [] CA.HelpFormatAll $ cmdArgsMode testArgs

{- Adapted from
 - https://hackage.haskell.org/package/formatting-6.1.1/docs/src/Formatting-Clock.html#timeSpecs
 - since we need a better GHC to use the proper formatters package.
 -}
timeDeltaStr :: TimeSpec -> TimeSpec -> String
timeDeltaStr (TimeSpec s1 n1) (TimeSpec s2 n2)
  | Just i <- scale ((10 ^ 9) * 60 * 60 * 24) = fixed i ++ " d"
  | Just i <- scale ((10 ^ 9) * 60 * 60) = fixed i ++ " h"
  | Just i <- scale ((10 ^ 9) * 60) = fixed i ++ " m"
  | Just i <- scale (10 ^ 9) = fixed i ++ " s"
  | Just i <- scale (10 ^ 6) = fixed i ++ " ms"
  | Just i <- scale (10 ^ 3) = fixed i ++ " µs"
  | otherwise = show diff ++ " ns"
  where
    fixed :: Double -> String
    fixed i = take 4 $ show i
    scale :: Integer -> Maybe Double
    scale i
      | diff >= i = Just (fromIntegral diff / fromIntegral i)
      | otherwise = Nothing
    diff :: Integer
    diff = a1 - a2
    a1 = (fromIntegral s1 * 10 ^ 9) + fromIntegral n1
    a2 = (fromIntegral s2 * 10 ^ 9) + fromIntegral n2

timeIt :: String -> IO a -> IO a
timeIt header action = do
  stTime <- getTime Monotonic
  a <- action
  endTime <- getTime Monotonic
  putStr header
  putStrLn $ timeDeltaStr endTime stTime
  return a

mainDB :: TestArgs -> IO ()
mainDB arg = do
  st <- timeIt "Open state: " $ openLocalState initialState
  _ <- case arg of
    List -> timeIt "List time: " $ dump st
    Clean -> timeIt "Clean time: " $ clean st
    GC -> timeIt "GC time: " $ createCheckpoint st >> createArchive st
    --Insert _ x -> timeIt "Insertion time: " $ insertDB st x
    --Sum -> timeIt "Sum computation: " $ sumDB st
    Search (Just k) -> timeIt "Search: " $ searchDB k st
    Size -> timeIt "Size computation: " $ sizeDB st
    _ -> error $ concat [show arg, " should be handled before this point"]
  timeIt "Close state: " $ closeAcidState st

dump :: AcidState (EventState QueryTest) -> IO ()
dump st = query st QueryTest >>= print

insertDB :: AcidState (EventState InsertTest) -> Int -> IO (EventResult InsertTest)
insertDB st = update st . InsertTest

clean :: AcidState (EventState CleanTest) -> IO (EventResult CleanTest)
clean st = update st CleanTest

searchDB :: String -> AcidState (EventState SearchTest) -> IO ()
searchDB k st = query st (SearchTest k) >>= print . maybe [] intVals

sumDB :: AcidState (EventState SumTest) -> IO ()
sumDB st = query st SumTest >>= print

sizeDB :: AcidState (EventState SizeTest) -> IO ()
sizeDB st = query st SizeTest >>= print

data TestArgs
  = Clean
  | GC
  | Insert { key :: Maybe String, number :: [Int] }
  | List
  | RevSearch { number :: [Int] }
  | Search { key :: Maybe String }
  | Size
  | Sum { key :: Maybe String}
  | Help
  deriving (Data, Typeable, Show)

testArgs :: TestArgs
testArgs = modes
  [ Clean &= help "Clean DB"
  , GC &= help "Garbage collect, reduce size of files to increase speed"
  , Insert { key = def &= typ "KEY", number = def &= args &= typ "NUMBER"} &= help "Insert/merge new entry"
  , List &= help "List entries"
  , RevSearch { number = def &= args &= typ "NUMBER" } &= help "Search for keys which contain a (set of) number(s)"
  , Search { key = def &= typ "KEY" &= argPos 0 } &= help "Search for numbers belonging to a key"
  , Size &= help "Return size of DB"
  , Sum {key = def &= typ "KEY" } &= help "Sum numbers from (key) entries"
  , Help &= help "Show this help message" &= auto
  ]
  &= help "Test acid-state library"
  &= program "test"
  &= versionArg [ignore]
  &= helpArg [ignore]

{-
 - Plan:
 -
 - 1. Have a DB with a collection of integers mapped to the same String
 - (Details) + counter
 - 2. Dump the entire database (List)
 - 3. Print size of database (Size)
 - 4. Insert new numbers (Insert)
 -    a. empty key => compute default key based on size, current counter
 -    b. given key => add to the same key as before
 - 5. Retrieve numbers for a key (Search)
 - 6. Sum numbers (Sum)
 -    a. no key => sum everything
 -    b. given key => sum only numbers belonging to the key
 - 7. Clear database (Clean)
 - 8. Garbage collect acid state (GC)
 - 9. Get keys for a number (RevSearch)
 -}

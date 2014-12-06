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
import Data.Time.Clock
import Data.Typeable
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

$(makeAcidic ''Test ['cleanTest, 'queryTest, 'sizeTest, 'sumTest, 'insertTest])

main :: IO ()
main = do
  args' <- cmdArgs testArgs
  case args' of
    Help -> showHelp
    _ -> mainDB args'

showHelp :: IO ()
showHelp = print $ CA.helpText [] CA.HelpFormatAll $ cmdArgsMode testArgs

timeIt :: String -> IO a -> IO a
timeIt header action = do
  stTime <- getCurrentTime
  a <- action
  endTime <- getCurrentTime
  putStr header
  print $ diffUTCTime endTime stTime
  return a

mainDB :: TestArgs -> IO ()
mainDB arg = do
  st <- timeIt "Open state: " $ openLocalState initialState
  _ <- case arg of
    List -> timeIt "List time: " $ dump st
    Clean -> timeIt "Clean time: " $ clean st
    GC -> timeIt "GC time: " $ createCheckpoint st >> createArchive st
    Insert x -> timeIt "Insertion time: " $ insertDB st x
    Sum -> timeIt "Sum computation: " $ sumDB st
    Size -> timeIt "Size computation: " $ sizeDB st
    _ -> error $ concat [show arg, " should be handled before this point"]
  timeIt "Close state: " $ closeAcidState st

dump :: AcidState (EventState QueryTest) -> IO ()
dump st = query st QueryTest >>= print

insertDB :: AcidState (EventState InsertTest) -> Int -> IO (EventResult InsertTest)
insertDB st = update st . InsertTest

clean :: AcidState (EventState CleanTest) -> IO (EventResult CleanTest)
clean st = update st CleanTest

sumDB :: AcidState (EventState SumTest) -> IO ()
sumDB st = query st SumTest >>= print

sizeDB :: AcidState (EventState SizeTest) -> IO ()
sizeDB st = query st SizeTest >>= print

data TestArgs
  = Clean
  | GC
  | Insert {number :: Int}
  | List
  | RevSearch
  | Search
  | Size
  | Sum
  | Help
  deriving (Data, Typeable, Show)

testArgs :: TestArgs
testArgs = modes
  [ Clean &= help "Clean DB"
  , GC &= help "Garbage collect, reduce size of files to increase speed"
  , Insert { number = def &= argPos 0 } &= help "Insert new entry"
  , List &= help "List entries"
  , RevSearch &= help "Search for keys which contain a number"
  , Search &= help "Search for numbers belonging to a key"
  , Size &= help "Return size of DB"
  , Sum &= help "Sum numbers from entries"
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

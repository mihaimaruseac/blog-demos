{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (put, modify)
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
  } deriving (Eq, Ord, Typeable)

$(deriveSafeCopy 0 'base ''Details)

instance Indexable Details where
  empty = ixSet
    [ ixFun $ \dt -> [stringVal dt]
    , ixFun $ \dt -> intVals dt
    ]

data Test = Test
  { nextID :: Int
  , details :: IxSet Details
  } deriving (Typeable)

$(deriveSafeCopy 0 'base ''Test)

initialState :: Test
initialState = Test 0 empty

cleanTest :: Update Test ()
cleanTest = put initialState

{-
queryTest :: Query Test0 Test0
queryTest = ask

sumTest :: Query Test0 Int
sumTest = sum . elems <$> ask

sizeTest :: Query Test0 Int
sizeTest = length . elems <$> ask

insertTest :: Int -> Update Test0 ()
insertTest x = modify (Test0 . (x:) . elems)

$(makeAcidic ''Test0 ['queryTest, 'cleanTest, 'insertTest, 'sumTest, 'sizeTest])
-}

$(makeAcidic ''Test ['cleanTest])

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
    -- List -> timeIt "List time: " $ dump st
    Clean -> timeIt "Clean time: " $ clean st
    GC -> timeIt "GC time: " $ createCheckpoint st >> createArchive st
    -- Insert x -> timeIt "Insertion time: " $ insert st x
    -- Sum -> timeIt "Sum computation: " $ sumDB st
    -- Size -> timeIt "Size computation: " $ size st
    _ -> error "Should be handled before this point"
  timeIt "Close state: " $ closeAcidState st

{-
dump :: AcidState (EventState QueryTest) -> IO ()
dump st = query st QueryTest >>= print

insert :: AcidState (EventState InsertTest) -> Int -> IO (EventResult InsertTest)
insert st = update st . InsertTest
-}

clean :: AcidState (EventState CleanTest) -> IO (EventResult CleanTest)
clean st = update st CleanTest

{-
sumDB :: AcidState (EventState SumTest) -> IO ()
sumDB st = query st SumTest >>= print

size :: AcidState (EventState SizeTest) -> IO ()
size st = query st SizeTest >>= print
  -}

data TestArgs
  = Clean
  | GC
  | Insert {number :: Int}
  | List
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
  , Size &= help "Return size of DB"
  , Sum &= help "Sum numbers from entries"
  , Help &= help "Show this help message" &= auto
  ]
  &= help "Test acid-state library"
  &= program "test"
  &= versionArg [ignore]
  &= helpArg [ignore]

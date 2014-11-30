{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (put, modify)
import Data.Acid
import Data.SafeCopy
import Data.Time.Clock
import Data.Typeable
import System.Console.CmdArgs

import qualified System.Console.CmdArgs.Explicit as CA

newtype Test0 = Test0 { elems :: [Int] }
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Test0)

cleanTest :: Update Test0()
cleanTest = put $ Test0 []

queryTest :: Query Test0 Test0
queryTest = ask

sumTest :: Query Test0 Int
sumTest = sum . elems <$> ask

sizeTest :: Query Test0 Int
sizeTest = length . elems <$> ask

insertTest :: Int -> Update Test0 ()
insertTest x = modify (Test0 . (x:) . elems)

$(makeAcidic ''Test0 ['queryTest, 'cleanTest, 'insertTest, 'sumTest, 'sizeTest])

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
  st <- timeIt "Open state: " $ openLocalState $ Test0 []
  _ <- case arg of
    List -> timeIt "List time: " $ dump st
    Clean -> timeIt "Clean time: " $ clean st
    GC -> timeIt "GC time: " $ createCheckpoint st >> createArchive st
    Insert x -> timeIt "Insertion time: " $ insert st x
    Sum -> timeIt "Sum computation: " $ sumDB st
    Size -> timeIt "Size computation: " $ size st
    _ -> error "Should be handled before this point"
  timeIt "Close state: " $ closeAcidState st

dump :: AcidState (EventState QueryTest) -> IO ()
dump st = query st QueryTest >>= print

insert :: AcidState (EventState InsertTest) -> Int -> IO (EventResult InsertTest)
insert st = update st . InsertTest

clean :: AcidState (EventState CleanTest) -> IO (EventResult CleanTest)
clean st = update st CleanTest

sumDB :: AcidState (EventState SumTest) -> IO ()
sumDB st = query st SumTest >>= print

size :: AcidState (EventState SizeTest) -> IO ()
size st = query st SizeTest >>= print

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
  [ Clean &= help "Clean DB (reset number list)"
  , GC &= help "Garbage collect, reduce size of files to increase speed"
  , Insert { number = def &= argPos 0 } &= help "Insert new number"
  , List &= help "List numbers"
  , Size &= help "Return size of DB"
  , Sum &= help "Sum numbers"
  , Help &= help "Show this help message" &= auto
  ]
  &= help "Test acid-state library"
  &= program "test"
  &= versionArg [ignore]
  &= helpArg [ignore]

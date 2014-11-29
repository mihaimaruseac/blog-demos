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

newtype Test = Test { elems :: [Int] }
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Test)

cleanTest :: Update Test()
cleanTest = put $ Test []

queryTest :: Query Test Test
queryTest = ask

sumTest :: Query Test Int
sumTest = sum . elems <$> ask

sizeTest :: Query Test Int
sizeTest = length . elems <$> ask

insertTest :: Int -> Update Test ()
insertTest x = modify (Test . (x:) . elems)

$(makeAcidic ''Test ['queryTest, 'cleanTest, 'insertTest, 'sumTest, 'sizeTest])

main :: IO ()
main = do
  args' <- cmdArgs testArgs
  case args' of
    Help -> showHelp
    _ -> mainDB args'

showHelp :: IO ()
showHelp = print $ CA.helpText [] CA.HelpFormatAll $ cmdArgsMode testArgs

mainDB :: TestArgs -> IO ()
mainDB arg = do
  stTime <- getCurrentTime
  st <- openLocalState $ Test []
  endTime <- getCurrentTime
  print $ diffUTCTime endTime stTime
  _ <- case arg of
    List -> dump st
    Clean -> clean st
    GC -> createCheckpoint st >> createArchive st
    Insert x -> insert st x
    Sum -> sumDB st
    Size -> size st
    _ -> error "Should be handled before this point"
  closeAcidState st

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

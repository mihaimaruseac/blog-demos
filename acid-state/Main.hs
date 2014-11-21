{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (put, modify)
import Data.Acid
import Data.SafeCopy
import Data.Typeable
import System.Console.CmdArgs

import qualified System.Console.CmdArgs.Explicit as CA

newtype Test = Test [Int] deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Test)

cleanTest :: Update Test()
cleanTest = put $ Test []

queryTest :: Query Test Test
queryTest = ask

sumTest :: Query Test Int
sumTest = (\(Test l) -> sum l) <$> ask

insertTest :: Int -> Update Test ()
insertTest x = modify (\(Test l) -> Test $ x:l)

$(makeAcidic ''Test ['queryTest, 'cleanTest, 'insertTest, 'sumTest])

main :: IO ()
main = do
  args' <- cmdArgs testArgs
  print args'
  case args' of
    Help -> showHelp
    _ -> mainDB args'

showHelp :: IO ()
showHelp = print $ CA.helpText [] CA.HelpFormatAll $ cmdArgsMode testArgs

mainDB :: TestArgs -> IO ()
mainDB arg = do
  st <- openLocalState $ Test []
  _ <- case arg of
    List -> dump st
    Clean -> clean st
    Insert x -> insert st x
    Sum -> sumDB st
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

data TestArgs
  = Insert {number :: Int}
  | Clean
  | Sum
  | List
  | Help
  deriving (Data, Typeable, Show)

testArgs :: TestArgs
testArgs = modes
  [ Insert { number = def &= argPos 0 } &= help "Insert new number"
  , Clean &= help "Clean DB (reset number list)"
  , Sum &= help "Sum numbers"
  , List &= help "List numbers"
  , Help &= help "Show this help message" &= auto
  ]
  &= help "Test acid-state library"
  &= program "test"
  &= versionArg [ignore]
  &= helpArg [ignore]

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.Reader (ask)
import Control.Monad.State (put)
import Data.Acid
import Data.SafeCopy
import Data.Typeable (Typeable)

newtype Test = Test [Int] deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Test)

writeTest :: Test -> Update Test ()
writeTest = put

queryTest :: Query Test Test
queryTest = ask

$(makeAcidic ''Test ['writeTest, 'queryTest])

main :: IO ()
main = do
  st <- openLocalState $ Test []
  dump st
  _ <- clean st
  dump st
  _ <- insert st 42
  dump st
  closeAcidState st

dump :: AcidState (EventState QueryTest) -> IO ()
dump st = query st QueryTest >>= print

insert :: AcidState (EventState WriteTest) -> Int -> IO (EventResult WriteTest)
insert st number = do
  (Test current) <- query st QueryTest
  update st . WriteTest . Test $ number : current

clean :: AcidState (EventState WriteTest) -> IO (EventResult WriteTest)
clean st = update st . WriteTest . Test $ []

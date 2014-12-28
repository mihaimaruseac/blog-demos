import Data.Conduit
import Control.Monad.IO.Class

source :: Source IO Int
source = do
  yield 1
  yield 2
  yield 3
  yield 4

conduit :: Conduit Int IO String
conduit = do
  -- Get all of the adjacent pairs from the stream
  mi1 <- await
  mi2 <- await
  case (mi1, mi2) of
    (Just i1, Just i2) -> do
      yield $ show (i1, i2)
      leftover i2
      conduit
    _ -> return ()

sink :: Sink String IO ()
sink = do
  mstr <- await
  case mstr of
    Nothing -> return ()
    Just str -> do
      liftIO $ putStrLn str
      sink

sourceList :: Monad m => [a] -> Source m a
sourceList = mapM_ yield

source' :: Source IO Int
source' = sourceList [1..4]

sink' :: Sink String IO ()
sink' = awaitForever $ liftIO . putStrLn

main = do
  source $$ conduit =$ sink
  source' $$ conduit =$ sink
  source' $$ conduit =$ sink'

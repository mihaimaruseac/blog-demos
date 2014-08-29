-- Just requests

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

-- a input
-- r result
newtype Pipe a r = Pipe {unPipe :: IO (PipeStep a r)}

data PipeStep a r
  = Pure r
  | Request (a -> Pipe a r) -- continuation

instance Monad (Pipe a) where
  return x = Pipe $ return (Pure x)
  x >>= f = Pipe $ do
              xstep <- unPipe x
              case xstep of
                Request k -> return $ Request (k >=> f)
                Pure r    -> unPipe $ f r

instance MonadIO (Pipe a) where
  liftIO io = Pipe $ Pure <$> io

request :: Pipe r r
request = Pipe . return $ Request return
                                  -- IO monad
                 -- Pipe monad

runPipe :: IO a -> Pipe a r -> IO r
runPipe input p = do
  step <- unPipe p
  case step of
    Request k -> input >>= \a -> runPipe input (k a)
    Pure r    -> return r

examplePipe :: Pipe Int ()
examplePipe = do
  x <- request
  y <- request
  liftIO $ print (x + y)

exampleInput :: IO Int
exampleInput = putStr "> " >> readLn

main :: IO ()
main = runPipe exampleInput examplePipe

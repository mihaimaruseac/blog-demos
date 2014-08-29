-- Just responses

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

-- b output
-- r result
newtype Pipe b r = Pipe {unPipe :: IO (PipeStep b r)}

data PipeStep b r
  = Pure r
  | Respond b (Pipe b r)

instance Monad (Pipe b) where
  return x = Pipe $ return (Pure x)
  x >>= f  = Pipe $ do
              xstep <- unPipe x
              case xstep of
                Respond b k -> return $ Respond b (k >>= f)
                Pure r      -> unPipe $ f r

instance MonadIO (Pipe b) where
  liftIO io = Pipe $ Pure <$> io

respond :: b -> Pipe b ()
respond b = Pipe . return $ Respond b (return ())
                                       -- IO monad
                  -- Pipe monad

runPipe :: (b -> IO ()) -> Pipe b r -> IO r
runPipe output p = do
  step <- unPipe p
  case step of
    Respond b k -> output b >> runPipe output k
    Pure r      -> return r

examplePipe :: Pipe Int ()
examplePipe = do
  respond 2 -- call the IO action with this arg
  liftIO $ putStr " Hello "
  respond 3
  liftIO $ putStr " world\n"

exampleOutput :: Int -> IO ()
exampleOutput n = replicateM_ n $ putChar '.'

main :: IO ()
main = runPipe exampleOutput examplePipe

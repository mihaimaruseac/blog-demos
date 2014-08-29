{-# LANGUAGE ScopedTypeVariables #-}

-- Basic Pipes

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Void

-- a input
-- b output
-- r result
newtype Pipe a b r = Pipe {unPipe :: IO (PipeStep a b r)}

data PipeStep a b r
  = Pure r
  | Request (a -> Pipe a b r)
  | Respond b (Pipe a b r)

instance Monad (Pipe a b) where
  return x = Pipe $ return (Pure x)
  x >>= f  = Pipe $ do
              xstep <- unPipe x
              case xstep of
                Request k   -> return $ Request (k >=> f)
                Respond b k -> return $ Respond b (k >>= f)
                Pure r      -> unPipe $ f r

instance MonadIO (Pipe a b) where
  liftIO io = Pipe $ Pure <$> io

(>->) :: forall a b c r . Pipe a b r -> Pipe b c r -> Pipe a c r
(>->) = goRight
  where
    -- run the right pipe
    goRight :: Pipe a b r -> Pipe b c r -> Pipe a c r
    goRight p q = Pipe $ do
      qstep <- unPipe q
      case qstep of
        Respond b k -> return $ Respond b (goRight p k)
        Pure r      -> return $ Pure r -- one pipe stops -> everything stops
        Request k   -> unPipe $ goLeft p k -- need value from left pipe

    -- run the left pipe
    goLeft :: Pipe a b r -> (b -> Pipe b c r) -> Pipe a c r
    goLeft p q = Pipe $ do
      pstep <- unPipe p
      case pstep of
        Request k   -> return $ Request (\a -> goLeft (k a) q)
        Pure r      -> return $ Pure r
        Respond b k -> unPipe $ goRight k (q b) -- go back to right pipe

respond :: b -> Pipe a b ()
respond b = Pipe . return $ Respond b (return ())
                                       -- IO monad
                  -- Pipe monad

request :: Pipe r b r
request = Pipe . return $ Request return
                                  -- IO monad
                 -- Pipe monad

-- we can only run pipes which provide () as input and return Void (nothing)
runPipe :: Pipe () Void r -> IO r
runPipe p = do
  step <- unPipe p
  case step of
    Pure r      -> return r
    Request k   -> runPipe (k ()) -- construct a () to unblock, () ~= nothing
    Respond b _ -> absurd b -- erroneous case, cannot produce a b anyway

prompter :: Pipe a Int r
prompter = do
  liftIO $ putStrLn "Hi!"
  forever $ do
    i <- liftIO $ putStr "> " >> readLn
    respond i

printer :: Pipe Int b r
printer = forever $ do
  i <- request
  liftIO $ print i

mapPipe :: (a -> b) -> Pipe a b r
mapPipe f = forever $ do
  a <- request
  respond $ f a

takePipe :: Int -> Pipe a a () -- () because it terminates
takePipe n = replicateM_ n $ do
  a <- request
  respond a

main1 :: IO ()
main1 = runPipe (prompter >-> printer)

main2 :: IO ()
main2 = runPipe (prompter >-> mapPipe (* 2) >-> printer)

main3 :: IO ()
main3 = runPipe (prompter >-> takePipe 3 >-> printer)

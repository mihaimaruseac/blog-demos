{-# LANGUAGE ScopedTypeVariables #-}

-- Exceptions

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Void

-- a input
-- b output
-- r result
newtype Pipe a b r = Pipe {unPipe :: IO (PipeStep a b r)}

type Finalizer = IO ()

data PipeStep a b r
  = Pure (Either SomeException r)
  | Request Finalizer (a -> Pipe a b r) -- upstream runs to terminate downstream
  | Respond Finalizer b (Pipe a b r) -- downstream should run to terminate upstream

instance Monad (Pipe a b) where
  return x = Pipe $ return (Pure $ Right x)
  x >>= f  = Pipe $ do
              xstep <- unPipe x
              case xstep of
                Request z k    -> return $ Request z (k >=> f)
                Respond z b k  -> return $ Respond z b (k >>= f)
                Pure (Right r) -> unPipe $ f r
                Pure (Left e)  -> return $ Pure (Left e)

instance MonadIO (Pipe a b) where
  liftIO io = Pipe $ Pure <$> try io

(>->) :: forall a b c r . Pipe a b r -> Pipe b c r -> Pipe a c r
(>->) = goRight (return ())
  where
    -- run the right pipe
    --         v-- upstream finalizer
    goRight :: Finalizer -> Pipe a b r -> Pipe b c r -> Pipe a c r
    goRight z p q = Pipe $ do
      qstep <- unPipe q
      case qstep of
        Respond z' b k -> return $ Respond (z >> z') b (goRight z p k)
        Pure r         -> z >> return (Pure r) -- one pipe stops -> everything stops
        Request z' k   -> unPipe $ goLeft z' p k -- need value from left pipe

    -- run the left pipe
    --        v-- downstream finalizer
    goLeft :: Finalizer -> Pipe a b r -> (b -> Pipe b c r) -> Pipe a c r
    goLeft z p q = Pipe $ do
      pstep <- unPipe p
      case pstep of
        Request z' k   -> return $ Request (z >> z') (\a -> goLeft z (k a) q)
        Pure r         -> z >> return (Pure r)
        Respond z' b k -> unPipe $ goRight z' k (q b) -- go back to right pipe

respond :: b -> Pipe a b ()
respond b = Pipe . return $ Respond (return ()) b (return ())
                                       -- IO monad
                  -- Pipe monad

request :: Pipe r b r
request = Pipe . return $ Request (return ()) return
                                  -- IO monad
                 -- Pipe monad

finallyP :: Finalizer -> Pipe a b r -> Pipe a b r
finallyP z p = Pipe $ do
  step <- unPipe p
  case step of
    Request z' k   -> return $ Request (z' >> z) (\a -> finallyP z (k a))
    Respond z' b k -> return $ Respond (z' >> z) b (finallyP z k)
    Pure r         -> z >> return (Pure r)

catchP :: Exception e => Pipe a b r -> (e -> Pipe  a b r) -> Pipe a b r
catchP p h = Pipe $ do
  step <- unPipe p
  case step of
    Pure (Left e)  -> case fromException e of
                        Just e' -> unPipe $ h e'
                        Nothing -> return $ Pure (Left e)
    Pure (Right r) -> return $ Pure (Right r)
    Request z k    -> return $ Request z (\a -> catchP (k a) h)
    Respond z b k  -> return $ Respond z b (catchP k h)

-- we can only run pipes which provide () as input and return Void (nothing)
runPipe :: Pipe () Void r -> IO r
runPipe p = do
  step <- unPipe p
  case step of
    Pure (Right r) -> return r
    Pure (Left e)  -> throw e
    Request z k    -> runPipe (k ()) -- construct a () to unblock, () ~= nothing
    Respond z b _  -> absurd b -- erroneous case, cannot produce a b anyway

-- simulate finalizers by printing closing tag
prompter :: Pipe a Int r
prompter = finallyP (putStrLn "</prompter>") $ do
  liftIO $ putStrLn "<prompter>"
  forever $ do
    i <- liftIO $ putStr "> " >> readLn
    respond i

-- simulate finalizers by printing closing tag
printer :: Pipe Int b r
printer = finallyP (putStrLn "</printer>") $ do
  liftIO $ putStrLn "<printer>"
  forever $ do
    i <- request
    liftIO $ print i

mapPipe :: (a -> b) -> Pipe a b r
mapPipe f = forever $ do
  a <- request
  respond $ f a

mapPipeM :: (a -> Pipe a b b) -> Pipe a b r
mapPipeM f = forever $ do
  a <- request
  b <- f a
  respond b

takePipe :: Int -> Pipe a a () -- () because it terminates
takePipe n = replicateM_ n $ do
  a <- request
  respond a

exampleHandler :: AsyncException -> Pipe a b ()
exampleHandler e = liftIO . putStrLn $ "How dare you interrupt me?!"

main1 :: IO ()
main1 = runPipe (prompter >-> takePipe 3 >-> printer)

main2 :: IO ()
main2 = runPipe (   ( forever (prompter >-> takePipe 3)
                      `catchP`
                      exampleHandler
                    )
                >-> printer)

main3 :: IO ()
main3 = runPipe (   forever (prompter >-> takePipe 3)
                >-> takePipe 4
                >-> printer)

main4 :: IO ()
main4 = runPipe (   forever (prompter >-> takePipe 3)
                >-> takePipe 2
                >-> printer)

-- intentionally slow fib as pipe
fib :: Int -> Pipe  a b Int
fib 0 = return 1
fib 1 = return 1
fib n = do
  n_2 <- fib $ n - 2
  n_1 <- fib $ n - 1
  return $ n_2  + n_1

main5 :: IO ()
main5 = runPipe (prompter >-> mapPipeM fib >-> printer)

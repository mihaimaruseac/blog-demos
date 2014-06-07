import Data.Char
import Data.List
import Data.String
import Network
import System.IO
import System.Time
import System.Exit
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Prelude hiding (catch)

server = "irc.freenode.org"
port = 6667
chan = "#rosedu"
nick = "irCri"

type Net = ReaderT Bot IO
data Bot = Bot
  { socket :: Handle
  , starttime :: ClockTime
  }

main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st = catch (runReaderT run st) ignore
    ignore :: IOError -> IO ()
    ignore = const $ return ()

connect :: IO Bot
connect = notify $ do
  t <- getClockTime
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  return (Bot h t)
    where
      notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

run :: Net ()
run = do
  write "NICK" nick
  write "USER" (nick++" 0 * :tutorial bot")
  write "JOIN" chan
  asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
  s <- init `fmap` io (hGetLine h)
  io (putStrLn s)
  if ping s then pong s else eval (clean s)
    where
      clean = drop 1 . dropWhile (/= ':') . drop 1
      ping x = "PING :" `isPrefixOf` x
      pong x = write "PONG" (':' : drop 6 x)

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
  h <- asks socket
  io $ hPrintf h "%s %s\r\n" s t
  io $ printf "> %s %s\n" s t

io :: IO a -> Net a
io = liftIO

{-
dropWord :: String -> String
dropWord = unwords . drop 1 . words
-}

eval :: String -> Net ()
eval "!mmquit" = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
--eval "!more" = privmsg "more is not implemented yet"
eval x
{-
  | "!help" `isPrefixOf` x = help $ dropWord x
  | "!ask" `isPrefixOf` x = ask' $ dropWord x
  | "!hgrep" `isPrefixOf` x = hgrep $ dropWord x
  | "!cgrep" `isPrefixOf` x = cgrep $ dropWord x
  | "!stag" `isPrefixOf` x = stag $ dropWord x
  | "!tags" `isPrefixOf` x = tags $ dropWord x
  | "!huser" `isPrefixOf` x = huser $ dropWord x
  | "!cuser" `isPrefixOf` x = cuser $ dropWord x
  | "!" `isPrefixOf` x = noSuchCmd
  -}
  | any isDigit x = spam . read. fst . span isDigit . snd . span (not . isDigit) $ x
  | otherwise = return ()

noSuchCmd :: Net ()
noSuchCmd = privmsg "No such command or invalid argument"

spam :: Integer -> Net ()
spam x = privmsg $ "Bug #" ++ (show $ x + 1)

{-
help :: String -> Net ()
help "" = do
  privmsg "irCri bot: IRC bot for Conversation Retrieval"
  privmsg "Commands are: quit, help, more, ask, hgrep, cgrep, stag, tags, huser, cuser"
help "quit" = privmsg "quit : Forces the bot to leave the channel."
help "help" = privmsg "help : Shows the generic help message."
help "more" = privmsg "more : Displays more lines from the last output."
help "ask" = privmsg "ask QUESTION : Retrieve conversations for QUESTION."
help "hgrep" = privmsg "hgrep PATTERN : Searches for PATTERN in conversation headers"
help "cgrep" = privmsg "cgrep PATTERN : Searches for PATTERN in entire conversation"
help "stag" = privmsg "stag TAG : Searches for conversations matching TAG"
help "tags" = privmsg "tags : Lists all tags"
help "huser" = privmsg "huser USER : Shows conversations initiated by USER"
help "cuser" = privmsg "cuser USER : Shows conversations in which USER participated"
help _ = noSuchCmd

ask' :: String -> Net ()
ask' _ = privmsg "Command not implemented yet"

hgrep :: String -> Net ()
hgrep _ = privmsg "Command not implemented yet"

cgrep :: String -> Net ()
cgrep _ = privmsg "Command not implemented yet"

stag :: String -> Net ()
stag _ = privmsg "Command not implemented yet"

tags :: String -> Net ()
tags _ = privmsg "Command not implemented yet"

huser :: String -> Net ()
huser _ = privmsg "Command not implemented yet"

cuser :: String -> Net ()
cuser _ = privmsg "Command not implemented yet"
-}

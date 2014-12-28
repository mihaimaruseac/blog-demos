import Data.Conduit
import qualified Data.Conduit.List as CL

source :: Source IO Int -- produces a stream of Ints
source = CL.sourceList [1..4]

sink :: Sink String IO () -- consumes a stream of Strings, no result
sink = CL.mapM_ putStrLn

conduit :: Conduit Int IO String -- converts Ints into Strings
conduit = CL.map show

double :: Monad m => Conduit Int m Int
double = CL.map (*2)

main = do
  source $= double $$ conduit =$ sink
  source $= double $= conduit $$ sink
  source $= double =$ conduit $$ sink

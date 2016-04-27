import           Control.Monad
import           Control.Monad.Identity (Identity)
import qualified Data.Attoparsec.Text   as Atto
import qualified Data.Text              as T
import           System.IO              as IO

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  a <- getLine
  print $ a

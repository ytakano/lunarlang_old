import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity (Identity)
import qualified MLType                 as MT
import           Text.Parsec            ((<?>))
import qualified Text.Parsec            as Parsec

{-
TERM  := BOOL | INT | FUN | VAR
BOOL  := `bool
INT   := `int
FUN   := fun \( TERM, TERM \)
VAR   := ID
-}

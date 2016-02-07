import           System.Environment
import           Text.Parsec
import           Text.Parsec.String

{- BNF of Lunar Lisp
list   ::= '\'(' sp args sp ')'

exprs  ::= expr exprs | ε
expr   ::= '(' sp term sp args ')' | '(' sp expr sp args sp ')'

args   ::= arg sp args | ε
arg    ::= expr | list | num | term

term   ::= nonum chars
chars  ::= utf8 chars | ε

num    ::= oct | hex | bin | digits | digits '.' digits
digits ::= digit digits | ε
hex    ::= 'x' digits | 'X' digits
bin    ::= 'b' digits | 'B' digits
oct    ::= '0' digits

reserv ::= '(' | ')' | '.' | '\'' | '"' | ';'
sp     ::= space sp | ε
space  ::= ' ' | '\r' | '\n' | '\t'
alpha  ::= [a-zA-Z]
digit  ::= [0-9]
op     ::= '+' | '/' | '~' | '!' | '@' | '#' | '$' | '%' |
           '^' | '^' | '*' | '-' | '=' | '_' | '<' | '>' | '|'
utf8   ::= alphabet | digit | op | multibyte character
nonum  ::= alphabet | op | multibyte character
-}


parseLunarLisps :: [IO String] -> IO ()
parseLunarLisps (x:xs) =
    do
        parseLunarLisp x
        parseLunarLisps(xs)
parseLunarLisps [] = do return ()

parseLunarLisp :: IO String -> IO ()
parseLunarLisp content =
    do
        str <- content
        putStrLn str

readFiles :: IO [IO String]
readFiles =
    do
        files <- getArgs
        return $ map readFile files

main =
    do
        contents <- readFiles
        parseLunarLisps contents
        putStr ""

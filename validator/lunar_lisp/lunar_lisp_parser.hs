{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Applicative
import           Control.Monad.Identity (Identity)
import           System.Environment
import           Text.Parsec            ((<?>))
import qualified Text.Parsec            as Parsec

{- BNF of Lunar Lisp
exprs   ::= expr exprs | ε
expr    ::= '(' sp term sp args ')' | '(' sp expr sp args sp ')'

list    ::= '\'(' sp args sp ')'

args    ::= arg sp args | ε
arg     ::= expr | list | num | term

term    ::= nonum chars
chars   ::= utf8 chars | ε

num     ::= hex | oct | bin | float | natural
float   ::= natural '.' digits
natural ::= nonzero digits
digits  ::= digit digits | ε
hex     ::= '0x' hexnunm hexs
hexs    ::= hexnum hexs | ε
oct     ::= '0o' octnum octs
octs    ::= octnum octs | ε
bin     ::= '0b' binum bins
bins    ::= binnum bins | ε

reserv  ::= '(' | ')' | '.' | '\'' | '"' | ';'
sp      ::= space sp | ε
space   ::= ' ' | '\r' | '\n' | '\t'
alpha   ::= [a-zA-Z]
digit   ::= [0-9]
nonzero ::= [1-9]
binnum  ::= '0' | '1'
octnum  ::= [0-7]
hexnum  ::= [0-9a-fA-F] 
op      ::= '+' | '/' | '~' | '!' | '@' | '#' | '$' | '%' | ':' |
            '^' | '^' | '*' | '-' | '=' | '_' | '<' | '>' | '|'
utf8    ::= alpha | digit | op | multibyte character
nonum   ::= alpha | op | multibyte character
-}

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule text = Parsec.parse rule "(source)" text

parseLunarLisps :: [String] -> [IO String] -> IO ()
parseLunarLisps (x:xs) (y:ys) =
    do
        parseLunarLisp x y
        parseLunarLisps xs ys
parseLunarLisps [] _ = do return ()

parseLunarLisp :: String -> IO String -> IO ()
parseLunarLisp file content =
    do
        str <- content
        putStrLn file
        putStrLn str

readFiles :: IO ([FilePath], [IO String])
readFiles =
    do
        files <- getArgs
        let contents = map readFile files
        return (files, contents)

main =
    do
        ret <- readFiles
        let (files, contents) = ret
        parseLunarLisps files contents
        putStr ""

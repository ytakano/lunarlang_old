{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Applicative
import           Control.Monad.Identity (Identity)
import           Data.List              (intercalate)
import           System.Environment
import           Text.Parsec            ((<?>))
import qualified Text.Parsec            as Parsec

{- BNF of Lunar Lisp
exprs   ::= expr sp0 exprs | ε
expr    ::= sexpr | list

sexpr   ::= '(' sp0 args sp0 ')' | '(' sp0 ')'
list    ::= '\'(' sp0 args sp0 ')' | '\'(' sp0 ')'

args    ::= arg spargs
spargs  ::= sp args | ε
arg     ::= expr | list | num | term

term    ::= nonum chars
chars   ::= char chars | ε

num     ::= hex | oct | bin | float | natural
float   ::= natural '.' digits
natural ::= nonzero digits
digits  ::= digit digits | ε
hex     ::= '0x' hexnum hexs
hexs    ::= hexnum hexs | ε
oct     ::= '0o' octnum octs
octs    ::= octnum octs | ε
bin     ::= '0b' binum bins
bins    ::= binnum bins | ε

reserv  ::= '(' | ')' | '.' | '\'' | '"' | ';'
sp      ::= space sp0
sp0     ::= space sp0 | ε
space   ::= ' ' | '\r' | '\n' | '\t'
alpha   ::= [a-zA-Z]
digit   ::= [0-9]
nonzero ::= [1-9]
binnum  ::= '0' | '1'
octnum  ::= [0-7]
hexnum  ::= [0-9a-fA-F]
op      ::= '+' | '/' | '~' | '!' | '@' | '#' | '$' | '%' | ':' |
            '^' | '*' | '-' | '=' | '_' | '<' | '>' | '|' | '&'
char    ::= digit | nonum
nonum   ::= alpha | op | multibyte character
-}

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule text = Parsec.parse rule "(source)" text

-- exprs   ::= expr sp0 exprs | ε
parseExprs =
    do
        expr <- parseExpr
        parseSp0
        exprs <- Parsec.try parseExprs <|> return ""
        return $ expr ++ exprs  

-- expr    ::= sexpr | list
parseExpr :: Parsec.Parsec String () String
parseExpr = parseSexpr <|> parseList

-- sexpr   ::= '(' sp0 args sp0 ')' | '(' sp0 ')'
parseSexpr :: Parsec.Parsec String () String
parseSexpr =
    do
        lp <- Parsec.char '('
        parseSp0
        args <- Parsec.try parseArgs <|> return ""
        parseSp0
        rp <- Parsec.char ')'
        return $ lp : args ++ [rp]

-- list    ::= '\'(' sp0 args sp0 ')' | '\'(' sp0 ')'
parseList :: Parsec.Parsec String () String
parseList =
    do
        lp <- Parsec.string "'("
        parseSp0
        args <- Parsec.try parseArgs <|> return ""
        parseSp0
        rp <- Parsec.char ')'
        return $ lp ++ args ++ [rp]

-- args    ::= arg spargs
parseArgs :: Parsec.Parsec String () String
parseArgs =
    do
        arg <- parseArg
        args <- Parsec.try parseSpargs <|> return ""
        return $ arg ++ args

-- spargs  ::= sp args | ε
parseSpargs :: Parsec.Parsec String () String
parseSpargs =
    do
        parseSp
        result <- parseArgs
        return $ ' ':result

-- arg     ::= expr | list | num | term
parseArg :: Parsec.Parsec String () String
parseArg = parseExpr <|> parseNum <|> parseTerm

-- sp      ::= space sp | ε
parseSp :: Parsec.Parsec String () ()
parseSp =
    do
        Parsec.many1 $ Parsec.oneOf " \r\n\t"
        return ()

-- sp0     ::= space sp0 | ε
-- space   ::= ' ' | '\r' | '\n' | '\t'
parseSp0 :: Parsec.Parsec String () Char
parseSp0 =
    do
        Parsec.many $ Parsec.oneOf " \r\n\t"
        return ' '

-- term    ::= nonum chars
-- chars   ::= char chars | ε
parseTerm :: Parsec.Parsec String () String
parseTerm =
    do
        h <- parseNonum
        t <- Parsec.many parseChar
        return $ h:t

-- char    ::= alpha | digit | op | multibyte character
parseChar :: Parsec.Parsec String () Char
parseChar = Parsec.digit <|> parseNonum

-- nonum   ::= alpha | op | multibyte character
parseNonum :: Parsec.Parsec String () Char
parseNonum = Parsec.letter <|> parseOp <|> Parsec.noneOf ['\0'..'\127']

-- op      ::= '+' | '/' | '~' | '!' | '@' | '#' | '$' | '%' | ':' |
--             '^' | '*' | '-' | '=' | '_' | '<' | '>' | '|' | '&'
parseOp :: Parsec.Parsec String () Char
parseOp = Parsec.oneOf "+/~!@#$%:^*-=_<>|&"

-- num     ::= hex | oct | bin | float | natural
parseNum :: Parsec.Parsec String () String
parseNum =
    do
        result <- Parsec.try (parseHex)   <|>
                  Parsec.try (parseOct)   <|>
                  Parsec.try (parseBin)   <|>
                  Parsec.try (parseFloat) <|>
                  parseNatural
        return result

-- float   ::= natural '.' digits
parseFloat :: Parsec.Parsec String () String
parseFloat =
    do
        n <- parseNatural
        d <- Parsec.char '.'
        f <- parseDigits
        return $ n ++ d:f

-- natural ::= nonzero digits
parseNatural :: Parsec.Parsec String () String
parseNatural =
    do
        h <- Parsec.oneOf ['1'..'9']
        t <- parseDigits
        return $ h:t

-- digits  ::= digit digits | ε
-- digit   ::= [0-9]
parseDigits :: Parsec.Parsec String () String
parseDigits = Parsec.many Parsec.digit

-- hex     ::= '0x' hexnunm hexs
-- hexs    ::= hexnum hexs | ε
-- hexnum  ::= [0-9a-fA-F]
parseHex :: Parsec.Parsec String () String
parseHex =
    do
        h <- Parsec.string "0x"
        t <- Parsec.many1 Parsec.hexDigit
        return $ h ++ t

-- oct     ::= '0o' octnum octs
-- octs    ::= octnum octs | ε
-- octnum  ::= [0-7]
parseOct :: Parsec.Parsec String () String
parseOct =
    do
        h <- Parsec.string "0o"
        t <- Parsec.many1 Parsec.octDigit
        return $ h ++ t

-- bin     ::= '0b' binum bins
-- bins    ::= binnum bins | ε
-- binnum  ::= '0' | '1'
parseBin :: Parsec.Parsec String () String
parseBin =
    do
        h <- Parsec.string "0b"
        t <- Parsec.many1 $ Parsec.oneOf "01"
        return $ h ++ t

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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Applicative
import           Control.Monad.Identity (Identity)
import           Data.List              (intercalate)
import           System.Environment
import           Text.Parsec            ((<?>))
import qualified Text.Parsec            as Parsec

{- BNF of Lunar Lisp
exprs   ::= sp0 | sp0 expr exprs
expr    ::= '(' sp0 args sp0 ')'

args    ::= arg spargs | ε
spargs  ::= sp args | ε
arg     ::= literal | minus | term | num | expr

term    ::= nonum chars | string
chars   ::= char chars | ε

literal ::= '\'' term | '\' expr
minus   ::= '-' float | '-' int | '-' term | '-'

string  ::= '"' multibyte characters '"'

num     ::= num0 | float | int
num0    ::= '0' hex | '0' oct | '0' bin | '0'
float   ::= int '.' digits
int     ::= nonzero digits | '0'
digits  ::= digit digits | ε
hex     ::= 'x' hexnum hexs
hexs    ::= hexnum hexs | ε
oct     ::= 'o' octnum octs
octs    ::= octnum octs | ε
bin     ::= 'b' binum bins
bins    ::= binnum bins | ε

reserv  ::= '(' | ')' | '.' | '"' | ';' | '\''
sp      ::= space sp0
sp0     ::= space sp0 | ε
space   ::= comment | ' ' | '\r' | '\n' | '\t'
comment ::= ';' any char newline | ';' any char EOF
alpha   ::= [a-zA-Z]
digit   ::= [0-9]
nonzero ::= [1-9]
binnum  ::= '0' | '1'
octnum  ::= [0-7]
hexnum  ::= [0-9a-fA-F]
op      ::= '+' | '/' | '~' | '!' | '@' | '#' | '$' | '%' | ':' | '?'
            '^' | '*' | '-' | '=' | '_' | '<' | '>' | '|' | '&'
char    ::= digit | nonum
nonum   ::= alpha | op | multibyte character
-}

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule text = Parsec.parse rule "(source)" text

-- exprs   ::= sp0 | sp0 expr exprs
parseExprs = Parsec.try parseEmpty <|> parseExprs2

parseExprs2 :: Parsec.Parsec String () String
parseExprs2 =
    do
        parseSp0
        expr <- parseExpr <|> return ""
        exprs <- parseExprs <|> return ""
        return $ expr ++ exprs

parseEmpty :: Parsec.Parsec String () String
parseEmpty =
    do
        parseSp0
        Parsec.eof
        return ""

-- expr    ::= '(' sp0 args sp0 ')'
parseExpr :: Parsec.Parsec String () String
parseExpr =
    do
        lp <- Parsec.char '('
        parseSp0
        args <- parseArgs
        parseSp0
        rp <- Parsec.char ')'
        return $ lp : args ++ [rp]

-- args    ::= arg spargs | ε
-- spargs  ::= sp args | ε
parseArgs :: Parsec.Parsec String () String
parseArgs =
    do
        arg  <- Parsec.try (lookAheadLP) <|> parseArg
        args <- Parsec.try (lookAheadLP) <|> parseSpargs
        return $ arg ++ args

lookAheadLP =
    do
        parseSp0
        Parsec.lookAhead (Parsec.char ')')
        return ""

-- spargs  ::= sp args | ε
parseSpargs :: Parsec.Parsec String () String
parseSpargs =
    do
        parseSp
        result <- parseArgs
        return $ ' ':result

-- arg     ::= literal | minus | term | num | expr
parseArg :: Parsec.Parsec String () String
parseArg = parseLiteral <|> parseMinus <|> parseTerm <|> parseNum <|> parseExpr

-- literal ::= '\'' term | '\' expr
parseLiteral :: Parsec.Parsec String () String
parseLiteral =
    do
        h <- Parsec.char '\''
        t <- parseTerm <|> parseExpr
        return $ h:t

-- minus   ::= '-' float | '-' int | '-' term | '-'
parseMinus :: Parsec.Parsec String () String
parseMinus =
    do
        h <- Parsec.char '-'
        t <- Parsec.try parseFloat <|> parseInt <|> parseTerm <|> lookAheadSpLP
        return $ h:t

-- sp      ::= space sp | ε
-- space   ::= comment | ' ' | '\r' | '\n' | '\t'
parseSp :: Parsec.Parsec String () String
parseSp = Parsec.many1 (parseComment <|> (Parsec.oneOf " \r\n\t"))

-- sp0     ::= space sp0 | ε
parseSp0 :: Parsec.Parsec String () String
parseSp0 = Parsec.many (parseComment <|> (Parsec.oneOf " \r\n\t"))

-- comment ::= ';' any char newline | ';' any char EOF
parseComment :: Parsec.Parsec String () Char
parseComment =
    do
        Parsec.char ';'
        Parsec.manyTill Parsec.anyChar parseNewlineEof
        return ' '

parseNewline :: Parsec.Parsec String () ()
parseNewline =
    do
        Parsec.newline
        return ()

parseNewlineEof :: Parsec.Parsec String () ()
parseNewlineEof = parseNewline <|> Parsec.eof

-- term    ::= nonum chars | string
-- chars   ::= char chars | ε
parseTerm :: Parsec.Parsec String () String
parseTerm = parseTermNonum <|> parseString

parseTermNonum :: Parsec.Parsec String () String
parseTermNonum =
    do
        h <- parseNonum
        t <- Parsec.many parseChar
        return $ h:t

-- string  ::= '"' multibyte characters '"'
parseString :: Parsec.Parsec String () String
parseString =
    do
        h <- Parsec.char '"'
        str <- Parsec.many (parseEscapeChar <|> parseNotQuote)
        t <- Parsec.char '"'
        return $ '"' : (concat str) ++ "\""

parseEscapeChar :: Parsec.Parsec String () String
parseEscapeChar =
    do
        h <- Parsec.char '\\'
        t <- Parsec.anyChar
        return [h, t]

parseNotQuote :: Parsec.Parsec String () String
parseNotQuote =
    do
        result <- Parsec.noneOf "\""
        return [result]

-- char    ::= alpha | digit | op | multibyte character
parseChar :: Parsec.Parsec String () Char
parseChar = Parsec.digit <|> parseNonum

-- nonum   ::= alpha | op | multibyte character
parseNonum :: Parsec.Parsec String () Char
parseNonum = Parsec.letter <|> parseOp <|> Parsec.noneOf ['\0'..'\127']

-- op      ::= '+' | '/' | '~' | '!' | '@' | '#' | '$' | '%' | ':' | '?'
--             '^' | '*' | '-' | '=' | '_' | '<' | '>' | '|' | '&'
parseOp :: Parsec.Parsec String () Char
parseOp = Parsec.oneOf "+/~!@#$%:?^*-=_<>|&"

-- num     ::= num0 | float | int
parseNum :: Parsec.Parsec String () String
parseNum =
    do
        result <- parseNum0 <|> Parsec.try parseFloat <|> parseInt
        return result

-- num0    ::= '0' hex | '0' oct | '0' bin | '0'
parseNum0 :: Parsec.Parsec String () String
parseNum0 =
    do
        h <- Parsec.char '0'
        t <- parseHex <|>
             parseOct <|>
             parseBin <|>
             lookAheadSpLP
        return $ h:t

lookAheadSpLP :: Parsec.Parsec String () String
lookAheadSpLP =
    do
        Parsec.lookAhead parseSp <|> lookAheadLP
        return ""

-- float   ::= natural '.' digits
parseFloat :: Parsec.Parsec String () String
parseFloat =
    do
        n <- parseInt
        d <- Parsec.char '.'
        f <- parseDigits
        return $ n ++ d:f

-- int ::= nonzero digits | '0'
parseInt :: Parsec.Parsec String () String
parseInt = parseInt2 <|> Parsec.string "0"

parseInt2 =
    do
        h <- Parsec.oneOf ['1'..'9']
        t <- parseDigits
        return $ h:t

-- digits  ::= digit digits | ε
-- digit   ::= [0-9]
parseDigits :: Parsec.Parsec String () String
parseDigits = Parsec.many Parsec.digit

-- hex     ::= 'x' hexnunm hexs
-- hexs    ::= hexnum hexs | ε
-- hexnum  ::= [0-9a-fA-F]
parseHex :: Parsec.Parsec String () String
parseHex =
    do
        h <- Parsec.char 'x'
        t <- Parsec.many1 Parsec.hexDigit
        return $ h:t

-- oct     ::= 'o' octnum octs
-- octs    ::= octnum octs | ε
-- octnum  ::= [0-7]
parseOct :: Parsec.Parsec String () String
parseOct =
    do
        h <- Parsec.char 'o'
        t <- Parsec.many1 Parsec.octDigit
        return $ h:t

-- bin     ::= 'b' binum bins
-- bins    ::= binnum bins | ε
-- binnum  ::= '0' | '1'
parseBin :: Parsec.Parsec String () String
parseBin =
    do
        h <- Parsec.char 'b'
        t <- Parsec.many1 $ Parsec.oneOf "01"
        return $ h:t

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
        let result = Parsec.parse parseExprs file str
        case result of
            Right v -> putStrLn (file ++ ": passed")
            Left err -> putStrLn (file ++ ": failed! " ++ show err)

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

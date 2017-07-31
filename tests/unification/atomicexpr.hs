{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AtomicExpr(Term(..), AtomicExpr(..), parseExpr, expr2str) where

{-
ATOMIC_EXPR := ID \( (TERM,)* TERM \)
TERM        := BOOL | INT | FUNC | CONST | VAR
BOOL        := true | false
FUNC        := ID \( (TERM,)* TERM \)
CONST       := `ID
VAR         := ID
-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity (Identity)
import           Text.Parsec            ((<?>))
import qualified Text.Parsec            as Parsec

data Term = TermBool  Bool   |
            TermInt   Int    |
            TermConst String |
            TermVar   String |
            TermFunc  String [Term] deriving (Show)

data AtomicExpr = AtomicExpr {id :: String, term :: [Term]} deriving (Show)

parseTerm = do
    term <- (Parsec.try parseFalse) <|>
            (Parsec.try parseTrue)  <|>
            (Parsec.try parseInt)   <|>
            (Parsec.try parseConst) <|>
            (Parsec.try parseFunc)  <|>
            parseVar
    return term

parseVar = do
    id <- parseID
    return $ TermVar id

parseTrue = do
    Parsec.string "true"
    return $ TermBool True

parseFalse = do
    Parsec.string "false"
    return $ TermBool False

parseInt = do
    minus <- Parsec.try $ Parsec.char '-' <|> return '+'
    num   <- Parsec.try parseZero <|> parseNum
    if minus == '-'
        then return $ TermInt (num * (-1))
        else return $ TermInt num

parseZero = do
    zero <- Parsec.char '0'
    return 0

parseNum = do
    h      <- Parsec.oneOf ['1'..'9']
    digits <- Parsec.many Parsec.digit
    return (read $ h:digits)

parseConst = do
    Parsec.char '`'
    id <- parseID
    return $ TermConst id

parseID = do
    h <- Parsec.letter
    t <- Parsec.many $ Parsec.alphaNum
    return $ h:t

parseTerms terms = do
    Parsec.spaces
    term <- parseTerm
    Parsec.spaces
    c <- Parsec.try $ Parsec.char ',' <|> return ')'
    if c == ','
        then parseTerms $ term:terms
        else return $ reverse $ term:terms

parseFunc = do
    id <- parseID
    Parsec.spaces
    Parsec.char '('
    Parsec.spaces
    terms <- parseTerms []
    Parsec.spaces
    Parsec.char ')'
    return $ TermFunc id terms

parseAtom = do
    Parsec.spaces
    id <- parseID
    Parsec.spaces
    Parsec.char '('
    Parsec.spaces
    terms <- parseTerms []
    Parsec.spaces
    Parsec.char ')'
    Parsec.spaces
    return $ AtomicExpr id terms

terms2str str [] =
    str
terms2str "" (h:t) =
    terms2str (term2str h) t
terms2str str (h:t) =
    terms2str (str ++ ", " ++ (term2str h)) t

expr2str expr = (AtomicExpr.id expr) ++ "(" ++ (terms2str "" $ term expr) ++ ")"

term2str (TermBool x) =
    case x of
        True  -> "true"
        False -> "false"
term2str (TermInt x)    = show x
term2str (TermConst x)  = "`" ++ x
term2str (TermVar x)    = x
term2str (TermFunc x y) = x ++ "(" ++ (terms2str "" y) ++ ")"

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule text = Parsec.parse rule "(stdin)" text

parseExpr :: String -> Either Parsec.ParseError AtomicExpr
parseExpr expr = parse parseAtom expr

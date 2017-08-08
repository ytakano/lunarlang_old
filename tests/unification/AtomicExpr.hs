{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AtomicExpr(Term(..), parse, term2str, sameTerms) where

{-
TERM  := BOOL | INT | FUNC | CONST | VAR
BOOL  := true | false
FUNC  := ID \( (TERM,)* TERM \)
CONST := `ID
VAR   := ID
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
            TermFunc  String [Term] deriving (Show, Eq)

sameTerms :: Term -> Term -> Bool
sameTerms (TermBool x)    (TermBool y)     = x == y
sameTerms (TermInt x)     (TermInt y)      = x == y
sameTerms (TermConst x)   (TermConst y)    = x == y
sameTerms (TermVar x)     (TermVar y)      = x == y
sameTerms (TermFunc id _) (TermFunc id' _) = id == id'
sameTerms _ _ = False

parserTop = do
    Parsec.spaces
    term <- parseTerm
    Parsec.spaces
    Parsec.eof
    return term

parseTerm = do
    term <- (Parsec.try parseFalse) <|>
            (Parsec.try parseTrue)  <|>
            (Parsec.try parseInt)   <|>
            (Parsec.try parseConst) <|>
            Parsec.try parseVarFunc
    return term

parseVarFunc = do
    id <- parseID
    Parsec.spaces
    c <- (Parsec.try $ Parsec.char '(') <|> return ' '
    if c == '('
        then parseFunc id
        else return $ TermVar id

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

parseFunc id = do
    Parsec.spaces
    terms <- parseTerms []
    Parsec.spaces
    Parsec.char ')'
    return $ TermFunc id terms

terms2str str [] =
    str
terms2str "" (h:t) =
    terms2str (term2str h) t
terms2str str (h:t) =
    terms2str (str ++ ", " ++ (term2str h)) t

term2str (TermBool x) =
    case x of
        True  -> "true"
        False -> "false"
term2str (TermInt x)    = show x
term2str (TermConst x)  = "`" ++ x
term2str (TermVar x)    = x
term2str (TermFunc x y) = x ++ "(" ++ (terms2str "" y) ++ ")"

parseHelper :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parseHelper rule text = Parsec.parse rule "(stdin)" text

parse :: String -> Either Parsec.ParseError Term
parse term = parseHelper parserTop term

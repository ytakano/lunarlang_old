{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SmallML(Expr(..), MLType(..), parse) where

{-
EXPR := VAR | NUM | BOOL | FUN | CALL | LET | FIX | IF
VAR  := alphabet (alphabet | num)*
NUM  := number
BOOL := true | false
FUN  := fun VAR = EXPR .
CALL := EXPR EXPR
LET  := let VAR = EXPR in EXPR
FIX  := fix VAR = EXPR .
IF   := if EXPR then EXPR else EXPR
-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity (Identity)
import           Text.Parsec            ((<?>))
import qualified Text.Parsec            as Parsec

data Expr = ExprVar  String           |
            ExprNum  Int              |
            ExprBool Bool             |
            ExprFun  String Expr      |
            ExprCall Expr Expr        |
            ExprLet  String Expr Expr |
            ExprFix  String Expr      |
            ExprIf   Expr Expr Expr   |
            ExprNone deriving (Show, Eq)

data MLType = TBool |
              TInt  |
              TVar  |
              TFun MLType MLType deriving (Show, Eq)

parserTop = do
    Parsec.spaces
    expr <- parseExpr
    Parsec.spaces
    Parsec.eof
    return expr

parseExpr = do
    expr1 <- (Parsec.try parseFalse) <|>
            (Parsec.try parseTrue)  <|>
            (Parsec.try parseInt)   <|>
            parseFunLetFixIfVar
    expr2 <- (Parsec.try parseCall) <|> return ExprNone
    return $ case expr2 of
        ExprNone -> expr1
        _ -> ExprCall expr1 expr2

parseCall = do
    Parsec.char ' '
    Parsec.spaces
    parseExpr

parseTrue = do
    Parsec.string "true"
    return $ ExprBool True

parseFalse = do
    Parsec.string "false"
    return $ ExprBool False

parseFunLetFixIfVar = do
    id <- parseID
    case id of
        "fun" -> parseFun
        "let" -> parseLet
        "fix" -> parseFix
        "if"  -> parseIf
        _     ->
            if reserved id
                then fail $ "unexpected " ++ id
                else return $ ExprVar id
            where
                reserved id
                    | id == "in"   = True
                    | id == "else" = True
                    | id == "then" = True
                    | otherwise    = False

parseID = do
    h <- Parsec.letter
    t <- Parsec.many $ Parsec.alphaNum
    return $ h:t

parseIf = do
    Parsec.char ' '
    Parsec.spaces

    expr1 <- parseExpr

    Parsec.char ' '
    Parsec.spaces
    Parsec.string "then"
    Parsec.char ' '
    Parsec.spaces

    expr2 <- parseExpr

    Parsec.char ' '
    Parsec.spaces
    Parsec.string "else"
    Parsec.char ' '
    Parsec.spaces

    expr3 <- parseExpr

    return $ ExprIf expr1 expr2 expr3

parseLet = do
    Parsec.char ' '
    Parsec.spaces

    id <- parseID

    Parsec.char ' '
    Parsec.spaces
    Parsec.char '='
    Parsec.char ' '
    Parsec.spaces

    expr1 <- parseExpr

    Parsec.char ' '
    Parsec.spaces
    Parsec.string "in"
    Parsec.char ' '
    Parsec.spaces

    expr2 <- parseExpr

    return $ ExprLet id expr1 expr2

parseFun = do
    Parsec.char ' '
    Parsec.spaces

    id <- parseID

    Parsec.char ' '
    Parsec.spaces
    Parsec.char '='
    Parsec.char ' '
    Parsec.spaces

    expr <- parseExpr

    Parsec.spaces
    Parsec.char '.'

    return $ ExprFun id expr

parseFix = do
    Parsec.char ' '
    Parsec.spaces

    id <- parseID

    Parsec.char ' '
    Parsec.spaces
    Parsec.char '='
    Parsec.char ' '
    Parsec.spaces

    expr <- parseExpr

    Parsec.spaces
    Parsec.char '.'

    return $ ExprFix id expr

parseInt = do
    minus <- Parsec.try $ Parsec.char '-' <|> return '+'
    num   <- Parsec.try parseZero <|> parseNum
    if minus == '-'
        then return $ ExprNum (num * (-1))
        else return $ ExprNum num

parseNum = do
    h      <- Parsec.oneOf ['1'..'9']
    digits <- Parsec.many Parsec.digit
    return (read $ h:digits)

parseZero = do
    zero <- Parsec.char '0'
    return 0

parseHelper :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parseHelper rule text = Parsec.parse rule "(stdin)" text

parse :: String -> Either Parsec.ParseError Expr
parse expr = parseHelper parserTop expr

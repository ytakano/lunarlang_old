{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module SmallML(Expr(..), parse, expr2str, uniqueVar) where

{-
EXPR := VAR | NUM | BOOL | FUN | CALL | LET | FIX | IF
VAR  := alphabet (alphabet | num)*
NUM  := number
BOOL := true | false
FUN  := fun VAR = EXPR .
CALL := EXPR EXPR
LET  := let VAR = EXPR in EXPR .
FIX  := fix VAR = EXPR .
IF   := if EXPR then EXPR else EXPR .
-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity (Identity)
import qualified Data.List              as L
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Debug.Trace
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

uniqueVar e = e' where (e', _) = uniqueVar2 e (S.fromList []) (M.fromList [])

uniqueVar2 :: Expr -> S.Set String -> M.Map String String -> (Expr, S.Set String)
uniqueVar2 (ExprLet var e1 e2) vars mapper = (ExprLet var' e1' e2', v2) where
    (var', vars') =
        if S.member var vars
            then makeVar var vars 0
            else (var, S.insert var vars)
    (e1', v1) = uniqueVar2 e1 vars' mapper
    (e2', v2) = uniqueVar2 e2 v1 (M.insert var var' mapper)
uniqueVar2 (ExprFun var e) vars mapper = (ExprFun var' e', v1) where
    (var', vars') =
        if S.member var vars
            then makeVar var vars 0
            else (var, S.insert var vars)
    (e', v1) = uniqueVar2 e vars' (M.insert var var' mapper)
uniqueVar2 (ExprFix var e) vars mapper = (ExprFix var' e', v1) where
    (var', vars') =
        if S.member var vars
            then makeVar var vars 0
            else (var, S.insert var vars)
    (e', v1) = uniqueVar2 e vars' (M.insert var var' mapper)
uniqueVar2 (ExprIf e1 e2 e3) vars mapper = (ExprIf e1' e2' e3', v3) where
    (e1', v1) = uniqueVar2 e1 vars mapper
    (e2', v2) = uniqueVar2 e2 v1 mapper
    (e3', v3) = uniqueVar2 e3 v2 mapper
uniqueVar2 (ExprCall e1 e2) vars mapper = (ExprCall e1' e2', v2) where
    (e1', v1) = uniqueVar2 e1 vars mapper
    (e2', v2) = uniqueVar2 e2 v1 mapper
uniqueVar2 (ExprVar var) vars mapper = (ExprVar var', S.insert var' vars) where
    var' =
        case M.lookup var mapper of
            Nothing -> var
            Just x  -> x
uniqueVar2 a vars _ = (a, vars)

makeVar :: String -> S.Set String -> Int -> (String, S.Set String)
makeVar var vars n
    | n == 0 =
        if S.member var vars
            then makeVar var vars 1
            else (var, S.insert var vars)
    | otherwise =
        if S.member var' vars
            then makeVar var vars (n + 1)
            else (var', S.insert var' vars)
        where
            var' = var ++ (show n)

expr2str (ExprVar s) = s
expr2str (ExprNum n) = show n
expr2str (ExprBool n)
    | n == True  = "true"
    | n == False = "false"
expr2str (ExprFun s e)     = "fun " ++ s ++ " = " ++ (expr2str e) ++ "."
expr2str (ExprCall e1 e2)  = (expr2str e1) ++ " " ++ (expr2str e2)
expr2str (ExprLet s e1 e2) = "let " ++ s ++ " = " ++ (expr2str e1) ++ " in " ++ (expr2str e2) ++ "."
expr2str (ExprFix s e)     = "fix " ++ s ++ " = " ++ (expr2str e) ++ "."
expr2str (ExprIf e1 e2 e3) = "if " ++ (expr2str e1) ++ " then " ++ (expr2str e2) ++ " else " ++ (expr2str e3) ++ "."
expr2str (ExprNone)        = "None"

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

    Parsec.spaces
    Parsec.char '.'

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

    Parsec.spaces
    Parsec.char '.'

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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TestUnification(parse) where

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

parserTop = do
    Parsec.spaces
    expr <- parseTerm
    Parsec.spaces
    Parsec.eof
    return expr

parseTerm = do
    term <- (Parsec.try parseBool) <|>
            (Parsec.try parseInt) <|>
            parseFunVar
    return term

parseFunVar = do
    id <- parseID
    if id == "fun"
        then parseFun
        else return $ MT.TVar id

parseFun = do
    Parsec.spaces
    Parsec.char '('
    Parsec.spaces

    t1 <- parseTerm

    Parsec.spaces
    Parsec.char ','
    Parsec.spaces

    t2 <- parseTerm

    Parsec.spaces
    Parsec.char ')'

    return $ MT.TFun t1 t2

parseBool = do
    Parsec.string "`bool"
    return MT.TBool

parseInt = do
    Parsec.string "`int"
    return MT.TInt

parseID = do
    h <- Parsec.letter
    t <- Parsec.many $ Parsec.alphaNum
    return $ h:t

parseHelper :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parseHelper rule text = Parsec.parse rule "(stdin)" text

parse :: String -> Either Parsec.ParseError MT.MLType
parse expr = parseHelper parserTop expr

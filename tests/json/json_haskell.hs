{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity (Identity)
import           System.IO              as IO
import           Text.Parsec            ((<?>))
import qualified Text.Parsec            as Parsec

{-
number        = [ minus ] int [ frac ] [ exp ]
decimal-point = %x2E                         ; .
digit1-9      = %x31-39                      ; 1-9
e             = %x65 / %x45                  ; e E
exp           = e [ minus / plus ] 1*DIGIT
frac          = decimal-point 1*DIGIT
int           = zero / ( digit1-9 *DIGIT )
minus         = %x2D                         ; -
plus          = %x2B                         ; +
zero          = %x30                         ; 0
-}

parse_frac =
  do
    point  <- Parsec.string "."
    digits <- Parsec.many1 Parsec.digit
    return $ point ++ digits

parse_digit1_9 =
  do
    h      <- Parsec.oneOf ['1'..'9']
    digits <- Parsec.many Parsec.digit
    return $ h:digits

parse_exp =
  do
    e      <- Parsec.string "e"
    sign   <- (Parsec.try $ Parsec.string "-") <|> (Parsec.try $ Parsec.string "+") <|> return []
    digits <- parse_digit1_9
    return $ e ++ sign ++ digits

parse_number =
  do
    minus <- Parsec.try $ Parsec.string "-" <|> return []
    int   <- Parsec.try $ Parsec.string "0" <|> parse_digit1_9
    frac  <- Parsec.try parse_frac <|> return []
    exp   <- Parsec.try $ parse_exp

    return $ minus ++ int ++ frac ++ exp

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule text = Parsec.parse rule "(stdin)" text

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  a <- getLine
  print $ parse parse_number a

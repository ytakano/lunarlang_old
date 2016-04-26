{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity (Identity)
import           Data.Char              as C
import           System.IO              as IO
import           Text.Parsec            ((<?>))
import qualified Text.Parsec            as Parsec

{-
value          = false / null / true / object / array / number / string
false          = %x66.61.6c.73.65              ; false
null           = %x6e.75.6c.6c                 ; null
true           = %x74.72.75.65                 ; true

number         = [ minus ] int [ frac ] [ exp ]
decimal-point  = %x2E                          ; .
digit1-9       = %x31-39                       ; 1-9
e              = %x65 / %x45                   ; e E
exp            = e [ minus / plus ] 1*DIGIT
frac           = decimal-point 1*DIGIT
int            = zero / ( digit1-9 *DIGIT )
minus          = %x2D                          ; -
plus           = %x2B                          ; +
zero           = %x30                          ; 0

string         = quotation-mark *char quotation-mark
char           = unescaped /
                   escape (
                     %x22 /                    ; "    quotation mark  U+0022
                     %x5C /                    ; \    reverse solidus U+005C
                     %x2F /                    ; /    solidus         U+002F
                     %x62 /                    ; b    backspace       U+0008
                     %x66 /                    ; f    form feed       U+000C
                     %x6E /                    ; n    line feed       U+000A
                     %x72 /                    ; r    carriage return U+000D
                     %x74 /                    ; t    tab             U+0009
                     %x75 4HEXDIG )            ; uXXXX                U+XXXX
escape         = %x5C                          ; \
quotation-mark = %x22                          ; "
unescaped      = %x20-21 / %x23-5B / %x5D-10FFFF
-}

parse_value =
  do
    str <- (Parsec.try parse_number) <|> parse_string
    return str

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
    minus <- (Parsec.try $ Parsec.string "-") <|> return ""
    int   <- (Parsec.try $ Parsec.string "0") <|> parse_digit1_9
    frac  <- (Parsec.try parse_frac) <|> return ""
    exp   <- Parsec.try parse_exp <|> return ""

    return $ minus ++ int ++ frac ++ exp

parse_string =
  do
    q1  <- Parsec.char '"'
    str <- Parsec.many parse_char
    q2  <- Parsec.char '"'
    return str

parse_char =
  do
    c <- (Parsec.try $ Parsec.satisfy is_unescaped) <|> parse_escaped
    return c

is_unescaped x
  | x == '\x20'                    = True
  | x == '\x21'                    = True
  | '\x23' <= x && x <= '\x5b'     = True
  | '\x5d' <= x && x <= '\x10ffff' = True
  | otherwise                      = False

is_hexdig x
  | '0' <= x && x <= '9' = True
  | 'a' <= x && x <= 'f' = True
  | 'A' <= x && x <= 'F' = True
  | otherwise            = False

hexdig2char h1 h2 h3 h4 =
  C.chr $ x1 + x2 + x3 + x4 where
    x1 = 16 * 16 * 16 * (C.ord h1)
    x2 = 16 * 16 * (C.ord h2)
    x3 = 16 * (C.ord h3)
    x4 = C.ord h4

parse_escaped =
  do
    s <- Parsec.char '\\'
    c <- Parsec.try $ Parsec.oneOf ['"', '\\', '/', 'b', 'f', 'n', 'r', 't'] <|> parse_4hexdig
    return c

parse_4hexdig =
  do
    u  <- Parsec.char 'u'
    h1 <- Parsec.satisfy is_hexdig
    h2 <- Parsec.satisfy is_hexdig
    h3 <- Parsec.satisfy is_hexdig
    h4 <- Parsec.satisfy is_hexdig
    return $ hexdig2char h1 h2 h3 h4

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule text = Parsec.parse rule "(stdin)" text

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  a <- getLine
  print $ parse parse_value a

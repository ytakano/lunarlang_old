{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity (Identity)
import qualified Data.Char              as C
import           System.IO              as IO
import           Text.Parsec            ((<?>))
import qualified Text.Parsec            as Parsec

{-
JSON-text       = ws value ws

begin-array     = ws %x5B ws                   ; [ left square bracket
begin-object    = ws %x7B ws                   ; { left curly bracket
end-array       = ws %x5D ws                   ; ] right square bracket
end-object      = ws %x7D ws                   ; } right curly bracket
name-separator  = ws %x3A ws                   ; : colon
value-separator = ws %x2C ws                   ; , comma

ws = *(
        %x20 /                                 ; Space
        %x09 /                                 ; Horizontal tab
        %x0A /                                 ; Line feed or New line
        %x0D )                                 ; Carriage return

value           = false / null / true / object / array / number / string
false           = %x66.61.6c.73.65             ; false
null            = %x6e.75.6c.6c                ; null
true            = %x74.72.75.65                ; true

object          = begin-object [ member *( value-separator member ) ] end-object
member          = string name-separator value

array           = begin-array [ value *( value-separator value ) ] end-array

number          = [ minus ] int [ frac ] [ exp ]
decimal-point   = %x2E                         ; .
digit1-9        = %x31-39                      ; 1-9
e               = %x65 / %x45                  ; e E
exp             = e [ minus / plus ] 1*DIGIT
frac            = decimal-point 1*DIGIT
int             = zero / ( digit1-9 *DIGIT )
minus           = %x2D                         ; -
plus            = %x2B                         ; +
zero            = %x30                         ; 0

string          = quotation-mark *char quotation-mark
char            = unescaped /
                    escape (
                      %x22 /                   ; "    quotation mark  U+0022
                      %x5C /                   ; \    reverse solidus U+005C
                      %x2F /                   ; /    solidus         U+002F
                      %x62 /                   ; b    backspace       U+0008
                      %x66 /                   ; f    form feed       U+000C
                      %x6E /                   ; n    line feed       U+000A
                      %x72 /                   ; r    carriage return U+000D
                      %x74 /                   ; t    tab             U+0009
                      %x75 4HEXDIG )           ; uXXXX                U+XXXX
escape          = %x5C                         ; \
quotation-mark  = %x22                         ; "
unescaped       = %x20-21 / %x23-5B / %x5D-10FFFF
-}

data JSON_VAL = JSON_Bool   Bool       |
                JSON_Double Double     |
                JSON_Null   ()         |
                JSON_String String     |
                JSON_Array  [JSON_VAL] |
                JSON_Object [(String, JSON_VAL)] deriving (Show)

parse_separator x = parse_ws >> (Parsec.char x) >> parse_ws >> return ()
parse_ws = (Parsec.many $ Parsec.oneOf ['\x20', '\x09', '\x0a', '\x0d']) >> return ()

parse_false = Parsec.string "false" >> (return $ JSON_Bool False)
parse_true  = Parsec.string "true"  >> (return $ JSON_Bool True)
parse_null  = Parsec.string "null"  >> (return $ JSON_Null ())

parse_value =
  do
    val <- (Parsec.try parse_false)  <|>
           (Parsec.try parse_null)   <|>
           (Parsec.try parse_true)   <|>
           (Parsec.try parse_object) <|>
           (Parsec.try parse_array)  <|>
           (Parsec.try parse_number) <|>
             do
               str <- parse_string
               return $ JSON_String str
    return val

parse_object =
  do
    _     <- parse_separator '{'
    mems  <- Parsec.try parse_members <|> (return $ JSON_Object [])
    _     <- parse_separator '}'
    return mems

parse_members =
  do
    h <- parse_member
    _ <- parse_ws
    t <- (Parsec.try $ Parsec.many parse_sp_member) <|> return []
    return $ JSON_Object $ h:t

parse_member =
  do
    key <- parse_string
    _   <- parse_separator ':'
    val <- parse_value
    return $ (key, val)

parse_sp_member = Parsec.char ',' >> parse_ws >> parse_member

parse_array =
  do
    _     <- parse_separator '['
    vals  <- Parsec.try parse_values <|> (return $ JSON_Array [])
    _     <- parse_separator ']'
    return vals

parse_values =
  do
    h <- parse_value
    _ <- parse_ws
    t <- (Parsec.try $ Parsec.many parse_sp_value) <|> return []
    return $ JSON_Array $ h:t

parse_sp_value = parse_ws >> Parsec.char ',' >> parse_ws >> parse_value

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
    return $ JSON_Double $ read (minus ++ int ++ frac ++ exp)

parse_string =
  do
    _   <- Parsec.char '"'
    str <- Parsec.many parse_char
    _   <- Parsec.char '"'
    return str

parse_char =
  do
    c <- (Parsec.try $ Parsec.satisfy is_unescaped) <|> parse_escaped
    return c
  where is_unescaped x | x == '\x20'                    = True
                       | x == '\x21'                    = True
                       | '\x23' <= x && x <= '\x5b'     = True
                       | '\x5d' <= x && x <= '\x10ffff' = True
                       | otherwise                      = False

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
  where is_hexdig x | '0' <= x && x <= '9' = True
                    | 'a' <= x && x <= 'f' = True
                    | 'A' <= x && x <= 'F' = True
                    | otherwise            = False

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule text = Parsec.parse rule "(stdin)" text

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  a <- getLine
  print $ parse parse_value a

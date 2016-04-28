{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity (Identity)
import qualified Data.Attoparsec.Text   as Atto
import qualified Data.Char              as C
import qualified Data.Text              as T
import           System.IO              as IO

data JSON_VAL = JSON_Bool   Bool       |
                JSON_Double Double     |
                JSON_Null   ()         |
                JSON_String String     |
                JSON_Array  [JSON_VAL] |
                JSON_Object [(String, JSON_VAL)] deriving (Show)

parse_separator x = parse_ws >> Atto.char x >> parse_ws >> return ()

parse_ws = (Atto.many' $ Atto.satisfy is_ws) >> return ()
  where is_ws c | c == '\x20' = True
                | c == '\x09' = True
                | c == '\x0a' = True
                | c == '\x0d' = True
                | otherwise   = False

parse_false = Atto.string "false" >> (return $ JSON_Bool False)
parse_true  = Atto.string "true"  >> (return $ JSON_Bool True)
parse_null  = Atto.string "null"  >> (return $ JSON_Null ())

parse_value =
  do
    val <- (Atto.try parse_false)  <|>
           (Atto.try parse_null)   <|>
           (Atto.try parse_true)   <|>
           (Atto.try parse_object) <|>
           (Atto.try parse_array)  <|>
           (Atto.try parse_number) <|>
             do
               str <- parse_string
               return $ JSON_String str
    return val

parse_object =
  do
    _     <- parse_separator '{'
    mems  <- Atto.try parse_members <|> (return $ JSON_Object [])
    _     <- parse_separator '}'
    return mems

parse_members =
  do
    h <- parse_member
    _ <- parse_ws
    t <- (Atto.try $ Atto.many' parse_sp_member) <|> return []
    return $ JSON_Object $ h:t

parse_member =
  do
    key <- parse_string
    _   <- parse_separator ':'
    val <- parse_value
    return $ (key, val)

parse_sp_member = Atto.char ',' >> parse_ws >> parse_member
parse_array =
  do
    _     <- parse_separator '['
    vals  <- Atto.try parse_values <|> (return $ JSON_Array [])
    _     <- parse_separator ']'
    return vals

parse_values =
  do
    h <- parse_value
    _ <- parse_ws
    t <- (Atto.try $ Atto.many' parse_sp_value) <|> return []
    return $ JSON_Array $ h:t

parse_sp_value = Atto.char ',' >> parse_ws >> parse_value

parse_number =
  do
    n <- Atto.double
    return $ JSON_Double n

parse_string =
  do
    _   <- Atto.char '"'
    str <- Atto.many' parse_char
    _   <- Atto.char '"'
    return str

parse_char =
  do
    c <- (Atto.try $ Atto.satisfy is_unescaped) <|> parse_escaped
    return c
  where is_unescaped x | x == '\x20'                    = True
                       | x == '\x21'                    = True
                       | '\x23' <= x && x <= '\x5b'     = True
                       | '\x5d' <= x && x <= '\x10ffff' = True
                       | otherwise                      = False

parse_escaped =
  do
    s <- Atto.char '\\'
    c <- Atto.try $ Atto.satisfy is_esc <|> parse_4hexdig
    return c
  where is_esc c | c == '"'  = True
                 | c == '\\' = True
                 | c == '/'  = True
                 | c == 'b'  = True
                 | c == 'f'  = True
                 | c == 'n'  = True
                 | c == 'r'  = True
                 | c == 't'  = True
                 | otherwise = False

hexdig2char h1 h2 h3 h4 =
  C.chr $ x1 + x2 + x3 + x4 where
    x1 = 16 * 16 * 16 * (C.ord h1)
    x2 = 16 * 16 * (C.ord h2)
    x3 = 16 * (C.ord h3)
    x4 = C.ord h4

parse_4hexdig =
  do
    u  <- Atto.char 'u'
    h1 <- Atto.satisfy is_hexdig
    h2 <- Atto.satisfy is_hexdig
    h3 <- Atto.satisfy is_hexdig
    h4 <- Atto.satisfy is_hexdig
    return $ hexdig2char h1 h2 h3 h4
  where is_hexdig x | '0' <= x && x <= '9' = True
                    | 'a' <= x && x <= 'f' = True
                    | 'A' <= x && x <= 'F' = True
                    | otherwise            = False

print_result (Atto.Partial p)    = print_result $ p ""
print_result (Atto.Done i r)     = print (i, r)
print_result (Atto.Fail i x1 x2) = print (x1, x2)

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  a <- getLine
  print_result $ Atto.parse parse_value $ T.pack a

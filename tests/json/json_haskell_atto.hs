{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity (Identity)
import qualified Data.Attoparsec.Text   as Atto
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
           --(Parsec.try parse_object) <?>
           (Atto.try parse_array)  <|>
           Atto.try parse_number
--             do
--               str <- parse_string
--               return $ JSON_String str
    return val

parse_array =
  do
    _     <- parse_separator '['
    vals  <- Atto.try parse_values <|> (return $ JSON_Array [])
    _     <- parse_separator ']'
    return vals

parse_values =
  do
    h <- parse_value
    t <- (Atto.try $ Atto.many' parse_sp_value) <|> return []
    return $ JSON_Array $ h:t

parse_sp_value = parse_separator ',' >> parse_value

parse_number =
  do
    n <- Atto.double
    return $ JSON_Double n

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  a <- getLine
  print $ Atto.parse parse_value $ T.pack a

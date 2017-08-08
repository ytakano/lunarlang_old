import qualified SmallML as ML
import qualified System.IO  as IO

getUserLines = do
    putStr "> "
    IO.hFlush IO.stdout
    line <- getLine
    expr <- return $ ML.parse line
    print expr
    getUserLines

main :: IO ()
main = do
    putStrLn "(an empty line unifies the expressions and Ctrl+c terminates this process)"
    getUserLines
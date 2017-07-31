import           AtomicExpr
import           System.IO  as IO

unify exprs = do
    putStrLn "unify"
    print exprs

getUserLines exprs = do
    putStr "> "
    hFlush stdout
    line <- getLine
    if line == ""
        then do
            unify $ reverse exprs
        else do
            expr <- return $ parseExpr line
            getUserLines $ expr:exprs

main :: IO ()
main = do
    putStrLn "(an empty input unifies expressions and Ctrl+c terminates this process)"
    getUserLines []

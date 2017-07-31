import           AtomicExpr as AE
import           System.IO  as IO

disagreementSet terms = terms

testExprIDs [] = do
    putStrLn "no expressions"
    return False
testExprIDs (h:t) = testExprIDs2 (AE.id h) t

testExprIDs2 id [] = do
    return True
testExprIDs2 id (h:t)  =
    if id == (AE.id h)
        then do
            testExprIDs2 id t
        else do
            putStrLn "cannot unify:"
            putStrLn $ "  " ++ id ++ " != " ++ (AE.id h)
            return False

printExprs [] = do
    return ()
printExprs (h:t) = do
    putStr "  "
    putStrLn $ expr2str h
    printExprs t

unify exprs = do
    putStrLn "unify"
    putStrLn "expressions:"
    printExprs exprs
    result <- testExprIDs exprs
    if result == True
        then do
            return $ concatMap (\x -> term x) exprs
            print exprs
        else do
            return ()

parseLine line exprs =
    printErr $ AE.parseExpr line
    where
        printErr result =
            case result of
            Left err -> do
                print err
                return exprs
            Right expr -> do
                return $ expr:exprs

getUserLines exprs = do
    putStr "> "
    hFlush stdout
    line <- getLine
    if line == ""
        then do
            unify $ reverse exprs
            getUserLines []
        else do
            e <- parseLine line exprs
            getUserLines e

main :: IO ()
main = do
    putStrLn "(an empty line unifies the expressions and Ctrl+c terminates this process)"
    getUserLines []

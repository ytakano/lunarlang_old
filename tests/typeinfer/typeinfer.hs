import qualified Control.Monad.State as S
import qualified Data.List           as L
import qualified SmallML             as ML
import qualified MLType              as MT
import qualified TestUnification     as TU
import           System.Environment  (getArgs)
import qualified System.IO           as IO

parseSmallML = do
    putStr "> "
    IO.hFlush IO.stdout
    line <- getLine
    result <- return $ ML.parse line
    case result of
        Right expr -> do
            print $ S.runStateT (MT.typing expr) ([], [])
        Left err -> do
            print err
--    printx t
--    print $ S.execState add1 0

--    print $ S.get t
    parseSmallML

printUnifier [] = do
    return ()
printUnifier (((MT.TVar v), term):t) = do
    putStr "  "
    putStr v
    putStr " -> "
    putStrLn $ MT.type2str term
    printUnifier t

testUnification term = do
    putStr "> "
    IO.hFlush IO.stdout
    line <- getLine
    result <- return $ TU.parse line
    case result of
        Right t -> do
            case term of
                Nothing -> testUnification $ Just t
                Just t' -> do
                    putStrLn "terms:"
                    putStr "  "
                    putStrLn $ MT.type2str t'
                    putStr "  "
                    putStrLn $ MT.type2str t
                    case MT.unify [(t', t)] of
                        Right u -> do
                            putStrLn "unified term:"
                            putStr "  "
                            putStrLn $ MT.type2str (MT.substituteTerm u t)
                            putStrLn "unifier:"
                            printUnifier u
                        Left  e -> do
                            putStr "  "
                            putStrLn e
                    testUnification Nothing
        Left err -> do
            print err
            testUnification term

main :: IO ()
main = do
    args <- getArgs
    case L.find (\x -> x == "-u") args of
        Nothing -> do
            putStrLn "(an empty line unifies the expressions and Ctrl+c terminates this process)"
            parseSmallML
        _ -> do
            putStrLn "entering the test mode of unification"
            putStrLn "(an empty line unifies the expressions and Ctrl+c terminates this process)"
            testUnification Nothing

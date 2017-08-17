import qualified Control.Monad.State as S
import qualified Data.List           as L
import qualified MLType              as MT
import qualified SmallML             as ML
import           System.Environment  (getArgs)
import qualified System.IO           as IO
import qualified TestUnification     as TU

printTypes [] = do
    return ()
printTypes (((MT.TVar ('$':_)), _):t) = printTypes t
printTypes (((MT.TVar k), a):t) = do
    printType k a
    printTypes t

printType k a = do
    putStr "  "
    putStr k
    putStr ": "
    putStrLn $ MT.type2str a

parseSmallML = do
    putStr "> "
    IO.hFlush IO.stdout
    line <- getLine
    result <- return $ ML.parse line
    case result of
        Right expr -> do
            let expr' = ML.uniqueVar expr in
                do
                    putStrLn "expression:"
                    putStr "  "
                    putStrLn $ ML.expr2str expr'
                    case S.runStateT (MT.typing expr') ([], []) of
                        Left  err -> putStrLn err
                        Right (t, (_, types)) -> do
                            putStrLn "types:"
                            printTypes types
                            putStrLn "result:"
                            putStr "  "
                            putStrLn $ MT.type2str t
        Left err -> do
            print err
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

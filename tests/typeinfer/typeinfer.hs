import qualified Control.Monad.State as S
import qualified Data.List           as L
import qualified SmallML             as ML
import qualified MLType              as MT
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
            --testUnification

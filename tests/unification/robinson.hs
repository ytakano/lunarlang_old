import           AtomicExpr as AE
import           Data.List
import           System.IO  as IO

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

unique = reverse . nub . reverse
allTheSameF xs func = and $ map (func (head xs)) (tail xs)

getDisagreementSetOfArgs :: [[AE.Term]] -> Either String (Maybe [AE.Term])
getDisagreementSetOfArgs argsList =
    if (length $ head argsList) == 0
        then Right Nothing
        else
            case getDisagreementSet $ map (\x -> head x) argsList of
                Right Nothing -> getDisagreementSetOfArgs $ map (\x -> tail x) argsList
                result -> result

getDisagreementSetOfFuncs :: [AE.Term] -> Either String (Maybe [AE.Term])
getDisagreementSetOfFuncs funcs =
    let argsList = map (\(AE.TermFunc _ x) -> x) funcs in
        if allTheSame (map length argsList) == True
            then getDisagreementSetOfArgs argsList
            else
                let AE.TermFunc id _ = head funcs in
                    Left $ "error: the number of arguments is different (function \"" ++ id ++ "\")"

getDisagreementSet :: [AE.Term] -> Either String (Maybe [AE.Term])
getDisagreementSet terms =
    if allTheSameF terms AE.sameTerms == True
        then
            case head terms of
                TermFunc _ _ -> getDisagreementSetOfFuncs terms
                _ -> Right Nothing
        else Right $ Just (unique terms)

printTerms [] = do
    return ()
printTerms (h:t) = do
    putStr "  "
    putStrLn $ term2str h
    printTerms t

unify terms = do
    putStrLn "disagreement set:"
    putStr "  "
    print $ getDisagreementSet terms

parseLine line exprs =
    printErr $ AE.parse line
    where
        printErr result =
            case result of
            Left err -> do
                print err
                return exprs
            Right expr -> do
                return $ expr:exprs

getUserLines terms = do
    putStr "> "
    hFlush stdout
    line <- getLine
    if line == ""
        then do
            putStrLn "terms:"
            printTerms terms
            print terms
            unify $ reverse $ unique terms
            getUserLines []
        else do
            e <- parseLine line terms
            getUserLines e

main :: IO ()
main = do
    putStrLn "(an empty line unifies the expressions and Ctrl+c terminates this process)"
    getUserLines []

import qualified AtomicExpr as AE
import qualified Data.List  as L
import qualified Data.Map   as M
import qualified System.IO  as IO

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

unique = reverse . L.nub . reverse
allTheSameF xs func = and $ map (func (head xs)) (tail xs)

hasVar :: AE.Term -> AE.Term -> Bool
hasVar (AE.TermVar id) (AE.TermVar id') = id == id'
hasVar var (AE.TermFunc _ args) = hasVar2 var args
hasVar _ _ = False

hasVar2 var [] = False
hasVar2 var (h:t) =
    if hasVar var h
        then True
        else hasVar2 var t

findVar [] = Nothing
findVar (h:t) =
    case h of
        AE.TermVar _ -> Just h
        _ -> findVar t

findTerm var (h:t) =
    if var == h
        then findTerm var t
        else h

getNewSubstituion :: [AE.Term] -> Maybe (AE.Term, AE.Term)
getNewSubstituion terms =
    let var  = findVar terms in
        case var of
            Nothing -> Nothing
            Just v  ->
                let term = findTerm v terms in
                    if hasVar v $ findTerm v terms
                        then Nothing
                        else Just (v, term)

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
getDisagreementSet [] = Left "error: no terms"
getDisagreementSet terms =
    if allTheSameF terms AE.sameTerms == True
        then
            case head terms of
                AE.TermFunc _ _ -> getDisagreementSetOfFuncs terms
                _ -> Right Nothing
        else Right $ Just (unique terms)


composition1 [] x = x
composition1 ((a, b):t) (c, d) =
    if d == a
        then (c, b)
        else composition1 t (c, d)

composition3 [] _ = True
composition3 ((a, _):t) (b, c) =
    if a == b
        then False
        else composition3 t (b, c)

getComposition :: [(AE.Term, AE.Term)] -> [(AE.Term, AE.Term)] -> [(AE.Term, AE.Term)]
getComposition x y =
    let s1 = map (composition1 y) x
        s2 = [(a, b) | (a, b) <-s1, a /= b]
        s3 = [z | z <- y, composition3 x z] in
            s2 ++ s3

printTerms [] = do
    return ()
printTerms (h:t) = do
    putStr "  "
    putStrLn $ AE.term2str h
    printTerms t

unify terms = do
    putStrLn "disagreement set:"
    putStr "  "
    ds <- return $ getDisagreementSet terms
    print ds
    putStrLn "new substituion:"
    putStr "  "
    ss <- return $ case ds of
        Left  str -> Left str
        Right Nothing -> Left "empty disagreement set"
        Right (Just x) -> Right $ getNewSubstituion x
    print ss
    putStrLn "composition:"
    putStr "  "
    cp <- return $ case ss of
        Right (Just x) -> Just $ getComposition [] [x]
        _ -> Nothing
    print cp


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
    IO.hFlush IO.stdout
    line <- getLine
    if line == ""
        then do
            putStrLn "terms:"
            t <- return $ reverse $ unique terms
            printTerms t
            unify t
            getUserLines []
        else do
            e <- parseLine line terms
            getUserLines e

main :: IO ()
main = do
    putStrLn "(an empty line unifies the expressions and Ctrl+c terminates this process)"
    getUserLines []

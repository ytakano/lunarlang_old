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

compose1 :: [(AE.Term, AE.Term)] -> (AE.Term, AE.Term) -> (AE.Term, AE.Term)
compose1 y (id, term) = (id, head $ substitute [term] y)

compose3 [] _ = True
compose3 ((a, _):t) (b, c) =
    if a == b
        then False
        else compose3 t (b, c)

compose :: [(AE.Term, AE.Term)] -> [(AE.Term, AE.Term)] -> [(AE.Term, AE.Term)]
compose x y = s2 ++ s3 where
    s1 = map (compose1 y) x
    s2 = [(a, b) | (a, b) <-s1, a /= b]
    s3 = [z | z <- y, compose3 x z]

printTerms [] = do
    return ()
printTerms (h:t) = do
    putStr "  "
    putStrLn $ AE.term2str h
    printTerms t

substitute = substitute1 []

substitute1 :: [AE.Term] -> [AE.Term] -> [(AE.Term, AE.Term)] -> [AE.Term]
substitute1 result [] sub = reverse result
substitute1 result (h:t) sub =
    substitute1 (term:result) t sub where
        term = substitute2 h sub

substitute2 :: AE.Term -> [(AE.Term, AE.Term)] -> AE.Term
substitute2 term [] = term
substitute2 (AE.TermVar id) sub =
        getTerm (AE.TermVar id) sub
    where
        getTerm term [] = term
        getTerm (AE.TermVar id) ((AE.TermVar id', term):t)
            | id == id' = term
            | otherwise = getTerm (AE.TermVar id) t
substitute2 (AE.TermFunc id terms) sub =
    let terms' = substitute1 [] terms sub in
        AE.TermFunc id terms'
substitute2 term _ = term

unify2 :: [AE.Term] -> [(AE.Term, AE.Term)] -> Either String (AE.Term, [(AE.Term, AE.Term)])
unify2 terms sub =
    let t1 = unique $ substitute terms sub in
        if length t1 == 1
            then Right (head t1, sub)
            else unify3 terms sub t1

unify3 :: [AE.Term] -> [(AE.Term, AE.Term)] -> [AE.Term] -> Either String (AE.Term, [(AE.Term, AE.Term)])
unify3 terms sub t1 =
    case getDisagreementSet t1 of
        Left str       -> Left str
        Right Nothing  -> Left "error: could not find a disagreement set"
        Right (Just d) ->
            case getNewSubstituion d of
                Nothing -> Left "error: could not find a new substitution"
                Just s  -> unify2 terms (compose sub [s])

unify terms = unify2 terms []

printResult (Left str) = do
    putStrLn str
printResult (Right (term, c)) = do
    putStrLn "unified term:"
    printTerms [term]
    putStrLn "unifier:"
    printSubstitution c

printSubstitution [] = return ()
printSubstitution ((AE.TermVar id, term):t) = do
    putStrLn $ "  " ++ id ++ " -> " ++ (AE.term2str term)
    printSubstitution t

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
    if line == "" && length terms /= 0
        then do
            putStrLn "terms:"
            t <- return $ reverse $ unique terms
            printTerms t
            printResult $ unify t
            getUserLines []
        else do
            e <- parseLine line terms
            getUserLines e

main :: IO ()
main = do
    putStrLn "(an empty line unifies the expressions and Ctrl+c terminates this process)"
    getUserLines []

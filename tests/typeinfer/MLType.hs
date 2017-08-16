{-# LANGUAGE FlexibleContexts #-}

module MLType(MLType(..), unify, typing, type2str, substituteTerm) where

import qualified Control.Monad.State as S
import qualified SmallML             as ML
import Debug.Trace

data MLType = TBool       |
              TInt        |
              TVar String |
              TFun MLType MLType deriving (Show, Eq)

type2str TBool = "`bool"
type2str TInt  = "`int"
type2str (TVar s) = s
type2str (TFun t1 t2) = "fun(" ++ (type2str t1) ++ ", " ++ (type2str t2) ++ ")"

deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
  | i == 0    = as
  | otherwise = a : deleteN (i-1) as

unify a = unify2 a a

unify2 :: [(MLType, MLType)] -> [(MLType, MLType)] -> Either String [(MLType, MLType)]
unify2 [] v = Right v
unify2 (h:t) v =
    case martelliMontanari h of
        Nothing        -> Left $ "cannot unify " ++ (show h)
        Just (Right x) -> unify2 v' v' where
            idx = (length v) - ((length t) + 1)
            v' = x ++ (deleteN idx v)
        Just (Left x)  ->
            if s
                then unify2 ret ret
                else unify2 t v
            where
                idx = (length v) - ((length t) + 1)
                (s, ret) = substitute idx 0 x v [] False

substituteTerm [] x = x
substituteTerm (h:t) x = substituteTerm t term where
    (_, term) = substitute2 h x

substitute :: Int -> Int -> (MLType, MLType) -> [(MLType, MLType)] -> [(MLType, MLType)] -> Bool -> (Bool, [(MLType, MLType)])
substitute idx1 idx2 _ [] ret s = (s, ret)
substitute idx1 idx2 a ((b, c):t) ret s
    | idx1 == idx2 = substitute idx1 (idx2 + 1) a t ((b, c):ret) s
    | otherwise =
        substitute idx1 (idx2 + 1) a t ((x, y):ret) (p || q || s) where
            (p, x) = substitute2 a b
            (q, y) = substitute2 a c

substitute2 :: (MLType, MLType) -> MLType -> (Bool, MLType)
substitute2 ((TVar a), t) (TVar b)
    | a == b    = (True, t)
    | otherwise = (False, (TVar b))
substitute2 a (TFun t1 t2) =
    (p || q, TFun x y) where
        (p, x) = substitute2 a t1
        (q, y) = substitute2 a t2
substitute2 _ x = (False, x)

martelliMontanari :: (MLType, MLType) -> Maybe (Either (MLType, MLType) [(MLType, MLType)])
martelliMontanari ((TFun a1 a2), (TFun b1 b2)) = Just $ Right [(a1, b1), (a2, b2)]
martelliMontanari ((TVar a), (TVar b))
    | a == b    = Just $ Right []
    | otherwise = Just $ Left ((TVar a), (TVar b)) -- substitute
martelliMontanari (a, (TVar b)) = Just $ Right [((TVar b), a)]
martelliMontanari ((TVar a), b) =
    if findVar a b
        then Nothing
        else Just $ Left ((TVar a), b) -- substitute
martelliMontanari (a, b)
    | a == b    = Just $ Right []
    | otherwise = Nothing

findVar :: String -> MLType -> Bool
findVar _ TBool = False
findVar _ TInt  = False
findVar a (TVar b)
    | a == b    = True
    | otherwise = False
findVar a (TFun b c) =
    x || y where
        x = findVar a b
        y = findVar a c

isRecursive k t = False

compose a b = compose2 a b []

compose2 :: [(MLType, MLType)] -> [(MLType, MLType)] -> [(MLType, MLType)] -> Either String [(MLType, MLType)]
compose2 [] _ ret = Right $ reverse ret
compose2 ((TVar k, a):t) s ret =
    case isRecursive k a' of
        True  -> Left $ "error: " ++ k ++ " is defined recursively"
        False -> compose2 t s ((TVar k, a'):ret)
    where
        a' = compose3 a s

compose3 :: MLType -> [(MLType, MLType)] -> MLType
compose3 TBool _ = TBool
compose3 TInt _  = TInt
compose3 x@(TVar a) s =
    case findType a s of
        Nothing -> x
        Just x' -> x'
compose3 (TFun t1 t2) s = TFun t1' t2' where
    t1' = compose3 t1 s
    t2' = compose3 t2 s

failTyping str = S.StateT $ \_ -> Left str

typing :: ML.Expr -> S.StateT ([(String, String)], [(MLType, MLType)]) (Either String) MLType
typing (ML.ExprBool _) = S.StateT $ \x -> Right (TBool, x)
typing (ML.ExprNum  _) = S.StateT $ \x -> Right (TInt, x)
typing (ML.ExprVar x)  = typingVar x
typing (ML.ExprIf e1 e2 e3) = typingIf e1 e2 e3
typing _ = S.StateT $ \x -> Left "under construction"
--    | (ML.ExprNum  _) = S.state $ \x -> ((Right TInt), x)
--    | (ExprVar  _) = state $ \x -> ((Right TVar), x)
--    | (ExprIf e1 e2 e3) = typingIf expr

composeState s = do
    (v, s0) <- S.get
    case compose s0 s of
        Left err -> failTyping err
        Right s1 -> do
            S.put (v, s1)

addState s = do
    (v, s0) <- S.get
    S.put (v, addState2 s0 s)

addState2 :: [(MLType, MLType)] -> [(MLType, MLType)] -> [(MLType, MLType)]
addState2 s [] = s
addState2 s (h@(TVar k, _):t) =
    if existTVar k s
        then addState2 s t
        else addState2 (h:s) t

typingIf e1 e2 e3 = do
    t1 <- typing e1
    case unify [(t1, TBool)] of
        Left  err -> failTyping $ err ++ "\n  \"" ++ (ML.expr2str e1) ++ "\" is not boolean"
        Right ret -> do
            composeState ret
            addState ret
            return TBool
    t2 <- typing e2
    t3 <- typing e3
    case unify [(t2, t3)] of
        Left  err -> failTyping $ err ++ "\n  \"" ++ (ML.expr2str e2) ++ "\" and\n  \"" ++ (ML.expr2str e3) ++ "\" are different type"
        Right ret -> do
            composeState ret
            addState ret
            return $ compose3 t3 ret

typingVar :: String -> S.StateT ([(String, String)], [(MLType, MLType)]) (Either String) MLType
typingVar var = do
    s <- S.get
    t <- getTypeVar var s
    return t

getTypeVar :: String -> ([(String, String)], [(MLType, MLType)]) -> S.StateT ([(String, String)], [(MLType, MLType)]) (Either String) MLType
getTypeVar var ([], tvars) = createTypeVar var tvars 0
getTypeVar v1 (((v2, v3):t), tvars)
    | v1 == v2 = do
        case findType v3 tvars of
            Nothing  -> fail $ "cannot find a type variable of " ++ v1
            Just ret -> return ret
    | otherwise = getTypeVar v1 (t, tvars)

findType _ [] = Nothing
findType v1 (((TVar v2), ret):t)
    | v1 == v2  = Just ret
    | otherwise = findType v1 t

createTypeVar var tvars 0 =
    if existTVar var tvars
        then createTypeVar var tvars 1
        else addTypeVar var var
createTypeVar var tvars n =
    let t = var ++ (show n) in
        if existTVar t tvars
            then createTypeVar var tvars (n + 1)
            else addTypeVar var t

addTypeVar var tvar = do
    (v, t) <- S.get
    S.put ((var, tvar):v, (TVar var, TVar tvar):t)
    return $ TVar tvar

existTVar _ [] = False
existTVar v1 ((TVar v2, _):t)
    | v1 == v2  = True
    | otherwise = existTVar v1 t

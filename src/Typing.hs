module Typing where

import UnionFind
import AST

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

-- types and instances --

data TypeError = Mismatch Type Type
               | OccursError String Type
               | UnboundVar String
               deriving(Eq, Ord, Show)

-- | the state will be maintained globally throughout checking.
--  This ensures the same union find is used throughout and newvars are always unique
type State = (UnionFind Type, Integer) -- the integer is used for newvar

-- | The monad for type checking. Contexts will transform locally, while States will transform globally.
--  Since checking may result in a type error, we include an either.
--  Every checker will take in a context and a state and output either a type error or the next state and a value.
newtype Checker a = Checker { runChecker :: Context -> State -> Either TypeError (State, a)}

instance Functor Checker where
    fmap f ma = Checker $ \ctx s ->
        case runChecker ma ctx s of
            Left err -> Left err
            Right (s',a) -> Right (s',f a)

instance Applicative Checker where
    pure a = Checker $ \_ s -> Right (s,a)
    mf <*> ma = Checker $ \ctx s ->
        case runChecker mf ctx s of
            Left err -> Left err
            Right (s',f) -> case runChecker ma ctx s' of
                Left err -> Left err
                Right (s'',a) -> Right (s'',f a)

instance Monad Checker where
    return = pure
    ma >>= k = Checker $ \ctx s ->
        case runChecker ma ctx s of
            Left err -> Left err
            Right (s',a) -> runChecker (k a) ctx s'

-- monad utilities --

-- | Throw a type error
throwError :: TypeError -> Checker a
throwError err = Checker $ \_ _ -> Left err

-- | Get the current context of the Checker
getContext :: Checker Context
getContext = Checker $ \ctx s -> Right (s,ctx)

-- | Locally modify the context for the given checking computation.
local :: (Context -> Context) -> Checker a -> Checker a
local f ma = Checker $ \ctx s -> runChecker ma (f ctx) s

-- | Locally add a variable annotation to the context for the given checking computation.
withVarAnnot :: String -> Scheme -> Checker a -> Checker a
withVarAnnot x s = local (Map.insert x s)

-- | Locally add variable annotations to the context fothe given checking computation.
withVarAnnots :: [(String, Scheme)] -> Checker a -> Checker a
withVarAnnots pairs = local (Map.union (Map.fromList pairs))

-- | Get the state of the Checker
get :: Checker State
get = Checker $ \_ s -> Right (s,s)

-- | Set the state of the checker
put :: State -> Checker ()
put s = Checker $ \_ _ -> Right (s,())

-- | Get the union find of the Checker
getUnionFind :: Checker (UnionFind Type)
getUnionFind = do
  (uf,_) <- get
  return uf

-- | Set the Checker's union find
setUnionFind :: UnionFind Type -> Checker ()
setUnionFind uf = do
    (_,n) <- get
    put (uf,n)

-- typing operations --

-- | Generate a unique type variable
newvar :: Checker Type
newvar = do
  (uf,n) <- get
  let t = TVar ("t"++show n)
  put (uf,n+1) -- ensure the next newvar is different
  return t

-- | Instantiate a type scheme to a mono type, replacing quantified variables with "newvar"
--  mono type variables
instantiate :: Scheme -> Checker Type
instantiate (SForall x s) = do
    x' <- newvar
    let s' = subScheme x x' s
    instantiate s'
instantiate (SMono t) = return t

-- | Generalize a mono type to its most general type scheme according to its and the context's
--  free variables
generalize :: Type -> Checker Scheme
generalize t = do
    ctx <- getContext
    let monoFrees = freeMonoVars t
        ctxFrees = freeCtxVars ctx
        frees = Set.difference monoFrees ctxFrees
    return (foldr SForall (SMono t) frees) -- forall all the free variables

-- | Assert the equality of two types and solve type variables as necessary
unify :: Type -> Type -> Checker ()
unify ta tb = do
    uf <- getUnionFind
    -- we want to use the "most solved" versions of these types
    let ta' = find uf ta
        tb' = find uf tb
    let unifyTVar x t
            | x `elem` freeMonoVars t = throwError (OccursError x t)
            | otherwise = setUnionFind (union uf t (TVar x))
            -- the order here is important. We want x's representative to be t, not the other way around
    case (ta',tb') of
        (TArr arg ret,TArr arg' ret') -> do
            unify arg arg'
            unify ret ret'
        (TInt, TInt) -> return () -- all good
        (TBool, TBool) -> return () -- all good
        (TVar x, t) -> unifyTVar x t
        (t, TVar x) -> unifyTVar x t
        (TArr _ _,_) -> throwError (Mismatch ta' tb')
        (TInt,_) -> throwError (Mismatch ta' tb')
        (TBool,_) -> throwError (Mismatch ta' tb')

-- type inference --

inferExpr :: Expr -> Checker Type
-- Var
inferExpr (Var x) = do
    ctx <- getContext
    case Map.lookup x ctx of
        Nothing -> throwError (UnboundVar x)
        Just s -> instantiate s
-- Int
inferExpr (Int _) = return TInt
-- Bool
inferExpr (Bool _) = return TBool
-- App
inferExpr (App f arg) = do
    tF <- inferExpr f
    tArg <- inferExpr arg
    tRet <- newvar
    unify (TArr tArg tRet) tF
    return tRet
-- Abs
inferExpr (Lambda x body) = do
    tArg <- newvar
    tRet <- withVarAnnot x (SMono tArg) (inferExpr body)
    return (TArr tArg tRet)
-- Let
inferExpr (Let x rhs body) = do
    tRhs <- inferExpr rhs
    sRhs <- generalize tRhs
    withVarAnnot x sRhs (inferExpr body)
-- If
inferExpr (If cnd thn els) = do
    tCnd <- inferExpr cnd
    unify TBool tCnd
    tThn <- inferExpr thn
    tEls <- inferExpr els
    unify tThn tEls
    return tThn

-- cleaning up types --

-- | Recursively find the fully solved form of this mono type
findMono :: Type -> Checker Type
findMono (TVar x) = do
    uf <- getUnionFind
    let x' = find uf (TVar x)
    if TVar x == x' then return (TVar x) else findMono x'
findMono TInt = return TInt
findMono TBool = return TBool
findMono (TArr arg ret) = do
    arg' <- findMono arg
    ret' <- findMono ret
    return (TArr arg' ret')

shortlex :: [a] -> [[a]]
shortlex xs = [[x] | x <- xs] ++ [xs' ++ [x] | xs' <- shortlex xs, x <- xs]

names :: [[Char]]
names = shortlex ['a'..'z']

-- | replace type variables such that the resulting type has variables appearing in alphabetical order
simplifyVars :: Type -> Type
simplifyVars t =
    let frees = Set.toList $ freeMonoVars t
        subs = Map.fromList $ zip frees (fmap TVar names)
        t' = subsMono subs t
    in t'

-- | solve the type and make its variables nice
finalizeMono :: Type -> Checker Type
finalizeMono t = do
    solved <- findMono t
    return (simplifyVars solved)

-- running --

-- | Run type inference on an expression under an empty context and state
runInferExpr :: Expr -> Either TypeError Type
runInferExpr e = case runChecker (finalizeMono =<< inferExpr e) mempty (mempty,1) of
    Left err -> Left err
    Right (_,t) -> Right t


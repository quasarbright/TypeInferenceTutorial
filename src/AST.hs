module AST where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

data Expr = Var String
          | Nat Integer
          | Bool Bool
          | Lambda String Expr
          | App Expr Expr
          | Let String Expr Expr
          | If Expr Expr Expr
          deriving(Eq, Ord, Show)

data Mono = TVar String
          | TNat
          | TBool
          | TArr Mono Mono
          deriving(Eq, Ord, Show)

data Scheme = SForall String Scheme
            | SMono Mono
            deriving(Eq, Ord, Show)

type Context = Map String Scheme

freeMonoVars :: Mono -> Set String
freeMonoVars = \case
    TVar x -> Set.singleton x
    TNat -> mempty
    TBool -> mempty
    TArr arg ret -> Set.union (freeMonoVars arg) (freeMonoVars ret)

freeSchemeVars :: Scheme -> Set String
freeSchemeVars = \case
    SForall x s -> Set.delete x (freeSchemeVars s)
    SMono t -> freeMonoVars t

freeCtxVars :: Context -> Set String
freeCtxVars ctx = mconcat (fmap freeSchemeVars (Map.elems ctx))

subsMono :: Map String Mono -> Mono -> Mono
subsMono subs = \case
    TVar x -> fromMaybe (TVar x) (Map.lookup x subs)
    TNat -> TNat
    TBool -> TBool
    TArr arg ret -> TArr (subsMono subs arg) (subsMono subs ret)

subMono :: String -> Mono -> Mono -> Mono
subMono target replacement = \case
    TVar x
        | x == target -> replacement
        | otherwise -> TVar x
    TNat -> TNat
    TBool -> TBool
    TArr arg ret -> TArr (subMono target replacement arg) (subMono target replacement ret)

subsScheme :: Map String Mono -> Scheme -> Scheme
subsScheme subs = \case
    SForall x s -> SForall x $ subsScheme subs' s
        where subs' = Map.delete x subs
    SMono t -> SMono $ subsMono subs t

subScheme :: String -> Mono -> Scheme -> Scheme
subScheme target replacement = \case
    SForall x s
        | x == target -> SForall x s
        | otherwise -> SForall x (subScheme target replacement s)
    SMono t -> SMono $ subMono target replacement t
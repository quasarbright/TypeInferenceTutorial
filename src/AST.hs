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
freeMonoVars (TVar x) = Set.singleton x
freeMonoVars TNat = Set.empty
freeMonoVars TBool = Set.empty
freeMonoVars (TArr arg ret) = Set.union (freeMonoVars arg) (freeMonoVars ret) 

freeSchemeVars :: Scheme -> Set String
freeSchemeVars (SForall x s) = Set.delete x (freeSchemeVars s)
freeSchemeVars (SMono t) = freeMonoVars t

freeCtxVars :: Context -> Set String
freeCtxVars ctx = mconcat (fmap freeSchemeVars (Map.elems ctx))

subsMono :: Map String Mono -> Mono -> Mono
subsMono subs (TVar x) = fromMaybe (TVar x) (Map.lookup x subs)
subsMono _    TNat = TNat
subsMono _    TBool = TBool
subsMono subs (TArr arg ret) = TArr (subsMono subs arg) (subsMono subs ret)

subMono :: String -> Mono -> Mono -> Mono
subMono target replacement (TVar x) =
    if x == target then replacement else TVar x
subMono _      _           TNat = TNat
subMono _      _           TBool = TBool
subMono target replacement (TArr arg ret) = TArr (subMono target replacement arg) (subMono target replacement ret)

subsScheme :: Map String Mono -> Scheme -> Scheme
subsScheme subs (SForall x s) =
    let subs' = Map.delete x subs
    in SForall x (subsScheme subs' s)
subsScheme subs (SMono t) = SMono (subsMono subs t)

subScheme :: String -> Mono -> Scheme -> Scheme
subScheme target replacement = \case
    SForall x s
        | x == target -> SForall x s
        | otherwise -> SForall x (subScheme target replacement s)
    SMono t -> SMono (subMono target replacement t)
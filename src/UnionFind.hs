module UnionFind where

import qualified Data.Map as Map
import Data.Map(Map)

type UnionFind a = Map a a

find :: Ord t => UnionFind t -> t -> t
find m k =
    case Map.lookup k m of
        Just rep
            | k == rep -> rep -- we are our own representative, done. 
            | otherwise -> find m rep -- follow the chain of representatives 
        Nothing -> k
    
union :: Ord k => UnionFind k -> k -> k -> UnionFind k
union m a b =
    let rep = find m a
        m' = Map.insert a rep (Map.insert b rep m)
    in m'
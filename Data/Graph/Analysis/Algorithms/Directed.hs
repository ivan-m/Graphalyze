module Data.Graph.Analysis.Algorithms.Directed where

import Data.Graph.Analysis.Graphalyze
import Data.Graph.Analysis.Algorithms.Undirected(componentsOf)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Basic
import Data.List
import Data.Maybe
import Control.Arrow((***), first)
import Control.Monad(join,liftM2)
import qualified Data.Map as M

rootsOf :: AGr a -> [LNode a]
rootsOf = filterNodes isRoot
    where
      isRoot g = null . pre g . fst

{-
classifyRoots    :: (Eq a) => GraphData a -> (Maybe (LNode a), [LNode a])
classifyRoots gr = first listToMaybe $ partition isWanted roots
    where
      roots = findRoots gr
      theRoot = wantedRoot gr
      isWanted = maybe (const False) (==) theRoot

wantedRootExists :: (Eq a) => GraphData a -> Bool
wantedRootExists = isJust . fst . classifyRoots
-}

--------------

leavesOf :: AGr a -> [LNode a]
leavesOf = filterNodes isLeaf
    where
      isLeaf g = null . suc g . fst

----

singletonsOf :: AGr a -> [LNode a]
singletonsOf = filterNodes isSingleton
    where
      isSingleton g = liftM2 (&&) (null . pre g) (null . suc g) . fst

----

coreOf :: (Eq a) => AGr a -> [AGr a]
coreOf = componentsOf . fixPoint strip
    where
      strip gr' = removeNodes roots $ removeNodes leaves $ gr'
          where
            roots = map node $ rootsOf gr'
            leaves = map node $ leavesOf gr'
            removeNode g = snd . flip match g
            removeNodes ns g = foldl' removeNode g ns

----

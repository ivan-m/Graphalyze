module Data.Graph.Analysis.Algorithms.Directed where

import Data.Graph.Analysis.Graphalyze
import Data.Graph.Analysis.Algorithms.Common
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Basic
import Data.List
import Data.Maybe
import Control.Arrow((***), first)
import Control.Monad(join,liftM2)
import qualified Data.Map as M

-- find those nodes where everything starts or stops
    
endNode        :: (AGr a -> Node -> [Node]) -> AGr a -> LNode a -> Bool
endNode f g ln = case (f g n) of
                   []   -> True
                   [n'] -> n' == n
                   _    -> False
    where
      n = node ln

rootsOf :: AGr a -> [LNode a]
rootsOf = filterNodes isRoot
    where
      isRoot = endNode pre

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
      isLeaf = endNode suc

----

singletonsOf :: AGr a -> [LNode a]
singletonsOf = filterNodes isSingleton
    where
      isSingleton = endNode neighbors

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

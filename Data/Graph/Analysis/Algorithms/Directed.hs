{- |
   Module      : Data.Graph.Analysis.Algorithms.Directed
   Description : Algorithms for directed graphs.
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   Defines algorithms that work on both directed graphs.
 -}
module Data.Graph.Analysis.Algorithms.Directed
    ( -- * Ending nodes
      endNode, endNode',
      endBy, endBy',
      -- ** Root nodes
      rootsOf, rootsOf',
      isRoot, isRoot',
      -- ** Leaf nodes
      leavesOf, leavesOf',
      isLeaf, isLeaf',
      -- ** Singleton nodes
      singletonsOf, singletonsOf',
      isSingleton, isSingleton',
      -- * Subgraphs
      coreOf,
    ) where

import Data.Graph.Analysis.Types
import Data.Graph.Analysis.Utils
import Data.Graph.Analysis.Algorithms.Common

import Data.Graph.Inductive.Graph
import Data.List

-- -----------------------------------------------------------------------------
{- |
   Find starting/ending nodes.

   We define an ending node as one where, given a function:
   > f :: (Graph g) => g a b -> Node -> [Node]
   the only allowed result is that node itself (to allow for loops).
 -}

-- | Determine if this 'LNode' is an ending node.
endNode :: (Graph g) => (g a b -> Node -> NGroup) -> g a b -> LNode a -> Bool
endNode f g ln = endNode' f g (node ln)

-- | Determine if this 'Node' is an ending node.
endNode'       :: (Graph g) => (g a b -> Node -> NGroup) -> g a b -> Node
               -> Bool
endNode' f g n = case (f g n) of
                  []   -> True
                  -- Allow loops
                  [n'] -> n' == n
                  _    -> False

-- | Find all 'LNode's that meet the ending criteria.
endBy   :: (Graph g) => (g a b -> Node -> NGroup) -> g a b -> LNGroup a
endBy f = filterNodes (endNode f)

-- | Find all 'Node's that match the ending criteria.
endBy'   :: (Graph g) => (g a b -> Node -> NGroup) -> g a b -> NGroup
endBy' f = filterNodes' (endNode' f)

-- -----------------------------------------------------------------------------

{- |
   Root detection.
 -}

-- | Find all roots of the graph.
rootsOf :: (Graph g) => g a b -> LNGroup a
rootsOf = endBy pre

-- | Find all roots of the graph.
rootsOf' :: (Graph g) => g a b -> NGroup
rootsOf' = endBy' pre

-- | Returns @True@ if this 'LNode' is a root.
isRoot :: (Graph g) => g a b -> LNode a -> Bool
isRoot = endNode pre

-- | Returns @True@ if this 'Node' is a root.
isRoot' :: (Graph g) => g a b -> Node -> Bool
isRoot' = endNode' pre

{-
classifyRoots    :: (Eq a) => GraphData a -> (Maybe (LNode a), LNGroup a)
classifyRoots gr = first listToMaybe $ partition isWanted roots
    where
      roots = findRoots gr
      theRoot = wantedRoot gr
      isWanted = maybe (const False) (==) theRoot

wantedRootExists :: (Eq a) => GraphData a -> Bool
wantedRootExists = isJust . fst . classifyRoots
-}


-- -----------------------------------------------------------------------------

{- |
   Leaf detection.
 -}

-- | Find all leaves of the graph.
leavesOf :: (Graph g) => g a b -> LNGroup a
leavesOf = endBy pre

-- | Find all leaves of the graph.
leavesOf' :: (Graph g) => g a b -> NGroup
leavesOf' = endBy' pre

-- | Returns @True@ if this 'LNode' is a leaf.
isLeaf :: (Graph g) => g a b -> LNode a -> Bool
isLeaf = endNode pre

-- | Returns @True@ if this 'Node' is a leaf.
isLeaf' :: (Graph g) => g a b -> Node -> Bool
isLeaf' = endNode' pre

-- -----------------------------------------------------------------------------

{- |
   Singleton detection.
 -}

-- | Find all singletons of the graph.
singletonsOf :: (Graph g) => g a b -> LNGroup a
singletonsOf = endBy pre

-- | Find all singletons of the graph.
singletonsOf' :: (Graph g) => g a b -> NGroup
singletonsOf' = endBy' pre

-- | Returns @True@ if this 'LNode' is a singleton.
isSingleton :: (Graph g) => g a b -> LNode a -> Bool
isSingleton = endNode pre

-- | Returns @True@ if this 'Node' is a singleton.
isSingleton' :: (Graph g) => g a b -> Node -> Bool
isSingleton' = endNode' pre

-- -----------------------------------------------------------------------------

{- |
   The /core/ of the graph is the part of the graph containing all the
   cycles, etc.  Depending on the context, it could be interpreted as
   the part of the graph where all the "work" is done.
 -}
coreOf :: (DynGraph g, Eq a, Eq b) => g a b -> [g a b]
coreOf = componentsOf . fixPointGraphs stripEnds
    where
      stripEnds gr' = delNodes roots $ delNodes leaves $ gr'
          where
            roots = rootsOf' gr'
            leaves = leavesOf' gr'
            delNodes ns g = foldl' (flip delNode) g ns

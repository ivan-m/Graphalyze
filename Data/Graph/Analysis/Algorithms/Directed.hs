{- |
   Module      : Data.Graph.Analysis.Algorithms.Directed
   Description : Algorithms for directed graphs.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   Defines algorithms that work on directed graphs.
 -}
module Data.Graph.Analysis.Algorithms.Directed
    ( -- * Ending nodes
      -- $ends
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
      -- * Clustering
      levelGraph,
      -- * Node accessibility
      accessibleFrom,
      accessibleFrom',
      accessibleOnlyFrom,
      accessibleOnlyFrom',
      -- * Other
      leafMinPaths,
      leafMinPaths'
    ) where

import Data.Graph.Analysis.Types
import Data.Graph.Analysis.Utils

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.BFS(esp)

import Data.List(minimumBy, unfoldr)
import Data.Function(on)
import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.Set as S
import Data.Set(Set)
import Control.Monad(ap)

-- -----------------------------------------------------------------------------
{- $ends
   Find starting/ending nodes.

   We define an ending node as one where, given a function:

   @
     f :: (Graph g) => g a b -> Node -> [Node]
   @

   the only allowed result is that node itself (to allow for loops).
 -}

-- | Determine if this 'LNode' is an ending node.
endNode     :: (Graph g) => (g a b -> Node -> NGroup)
               -> g a b -> LNode a -> Bool
endNode f g = endNode' f g . node

-- | Determine if this 'Node' is an ending node.
endNode'       :: (Graph g) => (g a b -> Node -> NGroup) -> g a b -> Node
               -> Bool
endNode' f g n = case (f g n) of
                  []   -> True
                  -- Allow loops
                  [n'] -> n' == n
                  _    -> False

-- | Find all 'LNode's that meet the ending criteria.
endBy :: (Graph g) => (g a b -> Node -> NGroup) -> g a b -> LNGroup a
endBy = filterNodes . endNode

-- | Find all 'Node's that match the ending criteria.
endBy' :: (Graph g) => (g a b -> Node -> NGroup) -> g a b -> NGroup
endBy' = filterNodes' . endNode'

-- -----------------------------------------------------------------------------

{-
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

-- -----------------------------------------------------------------------------

{-
   Leaf detection.
 -}

-- | Find all leaves of the graph.
leavesOf :: (Graph g) => g a b -> LNGroup a
leavesOf = endBy suc

-- | Find all leaves of the graph.
leavesOf' :: (Graph g) => g a b -> NGroup
leavesOf' = endBy' suc

-- | Returns @True@ if this 'LNode' is a leaf.
isLeaf :: (Graph g) => g a b -> LNode a -> Bool
isLeaf = endNode suc

-- | Returns @True@ if this 'Node' is a leaf.
isLeaf' :: (Graph g) => g a b -> Node -> Bool
isLeaf' = endNode' suc

-- -----------------------------------------------------------------------------

{-
   Singleton detection.
 -}

-- | Find all singletons of the graph.
singletonsOf :: (Graph g) => g a b -> LNGroup a
singletonsOf = endBy neighbors

-- | Find all singletons of the graph.
singletonsOf' :: (Graph g) => g a b -> NGroup
singletonsOf' = endBy' neighbors

-- | Returns @True@ if this 'LNode' is a singleton.
isSingleton :: (Graph g) => g a b -> LNode a -> Bool
isSingleton = endNode neighbors

-- | Returns @True@ if this 'Node' is a singleton.
isSingleton' :: (Graph g) => g a b -> Node -> Bool
isSingleton' = endNode' neighbors

-- -----------------------------------------------------------------------------

{- |
   The /core/ of the graph is the part of the graph containing all the
   cycles, etc.  Depending on the context, it could be interpreted as
   the part of the graph where all the "work" is done.
 -}
coreOf :: (DynGraph g, Eq a, Eq b) => g a b -> g a b
coreOf = fixPointGraphs stripEnds
    where
      stripEnds gr' = delNodes roots . delNodes leaves $ gr'
          where
            roots = rootsOf' gr'
            leaves = leavesOf' gr'

-- -----------------------------------------------------------------------------

{- |
   Cluster the nodes in the graph based upon how far away they are
   from a root node.  Root nodes are in the cluster labelled "0",
   nodes in level "n" are at least /n/ edges away from a root node.
-}
levelGraph   :: (Ord a) => (DynGraph g) => g a b -> g (GenCluster a) b
levelGraph g = gmap addLbl g
    where
      lvls = zip [0..] . map S.toList $ graphLevels g
      lvMap = M.fromList
              $ concatMap (\(l,ns) -> map (flip (,) l) ns) lvls
      mkLbl n l = GC { clust = lvMap M.! n
                     , nLbl  = l
                     }

      addLbl (p,n,l,s) = (p, n, mkLbl n l, s)

type NSet = Set Node

-- | Obtain the levels in the graph.
graphLevels :: (Graph g) => g a b -> [NSet]
graphLevels = ap graphLevels' (S.fromList . rootsOf')

graphLevels'   :: (Graph g) => g a b -> NSet -> [NSet]
graphLevels' g = unfoldr getNextLevel . flip (,) g

-- | The @(NSet, g a b)@ parameters are the current nodes to be
--   starting with in the current graph.
getNextLevel :: (Graph g) => (NSet, g a b)
                -> Maybe (NSet, (NSet, g a b))
getNextLevel (ns,g)
    | S.null ns = Nothing
    | otherwise = Just (ns, (ns', g'))
    where
      g' = delNodes (S.toList ns) g
      ns' = flip S.difference ns
            . S.unions . S.toList
            $ S.map getSuc ns
      getSuc = S.fromList . suc g

-- -----------------------------------------------------------------------------

{- |
   The shortest paths to each of the leaves in the graph (excluding
   singletons).  This can be used to obtain an indication of the
   overall height/depth of the graph.
 -}
leafMinPaths   :: (Graph g) => g a b -> [LNGroup a]
leafMinPaths g = map (lfMinPth g rs) ls
    where
      rs = rootsOf' g
      ls = leavesOf' g

{- |
   The shortest paths to each of the leaves in the graph (excluding
   singletons).  This can be used to obtain an indication of the
   overall height/depth of the graph.
 -}
leafMinPaths' :: (Graph g) => g a b -> [NGroup]
leafMinPaths' = map (map node) . leafMinPaths

-- | Given the list of roots in this graph, find the shortest path to
--   this leaf node.
lfMinPth        :: (Graph g) => g a b -> [Node] -> Node -> LNGroup a
lfMinPth g rs l = addLabels g
                  . snd
                  . minimumBy (compare `on` fst)
                  . addLengths
                  $ map (\ r -> esp r l g) rs

-- -----------------------------------------------------------------------------

-- | Find all 'Node's that can be reached from the provided 'Node's.
accessibleFrom   :: (Graph g) => g a b -> [Node] -> [Node]
accessibleFrom g = S.toList . accessibleFrom' g . S.fromList

-- | Find all 'Node's that can be reached from the provided nodes
--   using 'Set's rather than lists.
accessibleFrom'   :: (Graph g) => g a b -> Set Node -> Set Node
accessibleFrom' g = S.unions . graphLevels' g

-- | Find those 'Node's that are reachable only from the provided
--   'Node's.
accessibleOnlyFrom   :: (Graph g) => g a b -> [Node] -> [Node]
accessibleOnlyFrom g = S.toList . accessibleOnlyFrom' g . S.fromList

-- | Find those 'Node's that are reachable only from the provided
--   'Node's, using 'Set's rather than lists.
accessibleOnlyFrom'   :: (Graph g) => g a b -> Set Node -> Set Node
accessibleOnlyFrom' g = M.keysSet
                        . fixPoint keepOnlyInternal
                        . setKeys (pre g)
                        . accessibleFrom' g

-- | Pseudo-inverse of 'M.keysSet'.
setKeys   :: (Ord a) => (a -> b) -> Set a -> Map a b
setKeys f = M.fromDistinctAscList . map (ap (,) f) . S.toAscList

-- | Removing nodes which have predecessors outside of this Map.
keepOnlyInternal :: Map Node NGroup -> Map Node NGroup
keepOnlyInternal = M.filter =<< onlyInternalPred

-- | Are these predecessor nodes all found within this Map?
onlyInternalPred :: Map Node NGroup -> NGroup -> Bool
onlyInternalPred = all . flip M.member

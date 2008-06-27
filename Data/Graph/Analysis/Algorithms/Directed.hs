module Data.Graph.Analysis.Algorithms.Directed where

import Data.Graph.Analysis.Graphalyze    
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Basic
import Data.List
import Data.Maybe
import Control.Arrow((***), first)
import Control.Monad(join)
import qualified Data.Map as M

findRoots    :: GraphData a -> [Surround a]
findRoots gr = gsel isRoot (graph gr)
    where
      isRoot ctxt = null (pre' ctxt)

findRootNodes :: GraphData a -> [LNode a]
findRootNodes = findNodesFor findRoots

classifyRoots    :: (Eq a) => GraphData a -> (Maybe (Surround a), [Surround a])
classifyRoots gr = first listToMaybe $ partition isWanted roots
    where
      roots = findRoots gr
      theRoot = wantedRoot gr
      isWanted = maybe (const False) go theRoot
          where
            go ln ctxt = ln == (labNode' ctxt)

classifyRootNodes :: (Eq a) => GraphData a -> (Maybe (LNode a), [LNode a])
classifyRootNodes = (nodeOf *** nodeOf) . classifyRoots

wantedRootExists :: (Eq a) => GraphData a -> Bool
wantedRootExists = isJust . fst . classifyRoots
                
findLeaves    :: GraphData a -> [Surround a]
findLeaves gr = gsel isLeaf (graph gr)
    where
      isLeaf ctxt = null (suc' ctxt)

findLeafNodes :: GraphData a -> [LNode a]
findLeafNodes = findNodesFor findLeaves

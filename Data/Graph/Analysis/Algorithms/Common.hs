module Data.Graph.Analysis.Algorithms.Common where

import Data.Graph.Analysis.Graphalyze    
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Query.SP
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Internal.RootPath
import Data.List
import Data.Maybe
import Control.Arrow((***), first)
import Control.Monad(join,liftM2,ap)
import qualified Data.Map as M

-- find connected components

-- inbuilt function in Query.DFS only returns list of nodes...
-- this returns the actual graphs, which is thus more usable IMHO.

componentsOf :: AGr a -> [AGr a]
componentsOf = unfoldr splitComponent

-- this might work, except that the contexts have to be filtered for -- invalid nodes first
-- map (buildGr . preorder) . dffWith' id

splitComponent :: AGr a -> Maybe (AGr a, AGr a)
splitComponent g
    | isEmpty g = Nothing
    | otherwise = Just .          -- Get the type right
                  first buildGr . -- Create the subgraph
                  extractNode .   -- Extract components of subgraph
                  first Just .    -- Getting the types right
                  matchAny $ g    -- Choose an arbitrary node to begin with

extractNode :: ADecomp a -> ([AContext a], AGr a)
extractNode (Nothing,gr) = ([],gr)
extractNode (Just ctxt, gr)
    | isEmpty gr = ([ctxt], empty)
    | otherwise  = first (ctxt:) $ foldl' nodeExtractor ([],gr) nbrs
    where
      nbrs = neighbors' ctxt

nodeExtractor :: ([AContext a], AGr a) -> Node -> ([AContext a], AGr a)
nodeExtractor cg@(cs,g) n
    | gelem n g = first (++ cs) . extractNode $ match n g
    | otherwise = cg

---

recursiveNodes :: AGr a -> [LNode a]
recursiveNodes = map labNode' . gsel hasLoop
    where
      hasLoop ctxt = liftM2 (||) ($ pre' ctxt) ($ suc' ctxt) $ (elem (node' ctxt))

--



canReach      :: PGr a -> Node -> [Node]
canReach gr n = map (node . head . pathValues) tr
    where
      tr = spTree n gr

--

nodeCliques       :: AGr a -> LNode a -> [LNode a]
nodeCliques gr ln = addLabels gr cls
    where
      n = node ln
      cls = filter (elem n . suc gr) (suc gr n)
          

----

cyclesIn   :: AGr a -> [APath a]
cyclesIn g = map (addLabels g) .
             concat . unfoldr findLoops . toPathTree $ g

findLoops :: PGr a -> Maybe ([Path],PGr a)
findLoops g
    | isEmpty g = Nothing
    | otherwise = Just . first (cyclesFor g) . matchAny $ g

cyclesFor     :: PGr a -> PContext a -> [Path]
cyclesFor g c = map ((:) n . reverse) .
                filter ((==) n . head) .
                concatMap (treeFor g) $ sucs
    where
      n = node' c
      sucs = suc' c

-- spTree only creates branches of shortest length... need to replace -- with a custom function
             
treeFor      :: PGr a -> Node -> [Path]
treeFor gr n = map (map fst . pathValues) . spTree n $ gr

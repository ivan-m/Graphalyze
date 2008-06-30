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

-- inbuilt function in Query.DFS only returns list of nodes, and only
-- the shortest paths.
-- this returns the actual graphs, which is thus more usable IMHO.

componentsOf :: AGr a -> [AGr a]
componentsOf = unfoldr splitComponent

-- map (buildGr . validLinks . preorder) . dffWith' id

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
             concat . unfoldr findCycles $ g

findCycles :: AGr a -> Maybe ([Path], AGr a)
findCycles g
    | isEmpty g = Nothing
    | otherwise = Just . getCycles . matchAny $ g
    where
      getCycles (ctx,g) = (cyclesFor (ctx, g), g)
             
treeFor      :: PGr a -> Node -> [Path]
treeFor gr n = map (map fst . pathValues) . spTree n $ gr

cyclesFor :: AGDecomp a -> [Path]
cyclesFor = filter isCycle . pathTree . first Just
    where
      isCycle p = (not $ single p) && ((head p) == (last p))

pathTree             :: ADecomp a -> [Path]
pathTree (Nothing,_) = []
pathTree (Just ct,g)
    | isEmpty g = []
    | null sucs = [[n]]
    | otherwise = (:) [n] . map (n:) . concatMap (subPathTree g') $ sucs
    where
      n = node' ct
      sucs = suc' ct
      -- Avoid infinite loops by not letting it continue any further
      ct' = makeLeaf ct
      g' = ct' & g

subPathTree      :: AGr a -> Node -> [Path]
subPathTree gr n = pathTree $ match n gr

-- Remove all outgoing edges
makeLeaf           :: AContext a -> AContext a
makeLeaf (p,n,a,s) = (p', n, a, [])
    where
      nEdge = ((),n)
      -- Ensure there isn't an edge (n,n)
      p' = delete nEdge p

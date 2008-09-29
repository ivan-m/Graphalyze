{- |
   Module      : Data.Graph.Analysis.Algorithms.Common
   Description : Algorithms for all graph types.
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   Defines algorithms that work on both undirected and
   directed graphs.
 -}
module Data.Graph.Analysis.Algorithms.Common
    ( -- * Graph decomposition
      -- $connected
      componentsOf,
      pathTree,
      -- * Clique Detection
      -- $cliques
      cliquesIn,
      cliquesIn',
      findRegular,
      isRegular,
      -- * Cycle Detection
      -- $cycles
      cyclesIn,
      cyclesIn',
      uniqueCycles,
      uniqueCycles',
      -- * Chain detection
      -- $chains
      chainsIn,
      chainsIn'
    ) where

import Data.Graph.Analysis.Types
import Data.Graph.Analysis.Utils

import Data.Graph.Inductive.Graph
-- For linking purposes.  This will throw a warning.
import Data.Graph.Inductive.Query.DFS(components)
import Data.List
import Data.Maybe
import Control.Arrow

-- -----------------------------------------------------------------------------

{- $connected
   Finding connected components.

   Whilst the FGL library does indeed have a function 'components'
   that returns the connected components of a graph, it returns each
   component as a list of 'Node's.  This implementation instead
   returns each component as a /graph/, which is much more useful.

   Connected components are found by choosing a random node, then
   recursively extracting all neighbours of that node until no more
   nodes can be removed.

   Note that for directed graphs, these are known as the /weakly/
   connected components.
-}

-- | Find all connected components of a graph.
componentsOf :: (DynGraph g) => g a b -> [g a b]
componentsOf = unfoldr splitComponent

-- | Find the next component and split it off from the graph.
splitComponent :: (DynGraph g) => g a b -> Maybe (g a b, g a b)
splitComponent g
    | isEmpty g = Nothing
    | otherwise = Just .          -- Get the type right
                  first buildGr . -- Create the subgraph
                  extractNode .   -- Extract components of subgraph
                  first Just .    -- Getting the types right
                  matchAny $ g    -- Choose an arbitrary node to begin with

-- | Extract the given node and all nodes it is transitively
--   connected to from the graph.
extractNode :: (DynGraph g) => Decomp g a b -> ([Context a b], g a b)
extractNode (Nothing,gr) = ([],gr)
extractNode (Just ctxt, gr)
    | isEmpty gr = ([ctxt], empty)
    | otherwise  = first (ctxt:) $ foldl' nodeExtractor ([],gr) nbrs
    where
      nbrs = neighbors' ctxt

-- | Helper function for 'extractNode' above.
nodeExtractor :: (DynGraph g) => ([Context a b], g a b) -> Node
              -> ([Context a b], g a b)
nodeExtractor cg@(cs,g) n
    | gelem n g = first (++ cs) . extractNode $ match n g
    | otherwise = cg

-- -----------------------------------------------------------------------------

-- | Find all possible paths from this given node, avoiding loops,
--   cycles, etc.
pathTree             :: (DynGraph g) => Decomp g a b -> [NGroup]
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
      subPathTree gr n' = pathTree $ match n' gr

-- | Remove all outgoing edges
makeLeaf           :: Context a b -> Context a b
makeLeaf (p,n,a,_) = (p', n, a, [])
    where
      -- Ensure there isn't an edge (n,n)
      p' = filter (\(_,n') -> n' /= n) p

-- -----------------------------------------------------------------------------
{- $cliques
   Clique detection routines.  Find cliques by taking out a node, and
   seeing which other nodes are all common neighbours (by both 'pre'
   and 'suc').
 -}

-- | Finds all cliques (i.e. maximal complete subgraphs) in the given graph.
cliquesIn    :: (Graph g) => g a b -> [[LNode a]]
cliquesIn gr = map (addLabels gr) (cliquesIn' gr)

-- | Finds all cliques in the graph, without including labels.
cliquesIn'    :: (Graph g) => g a b -> [NGroup]
cliquesIn' gr = filter (isClique gr) (findRegular gr)

-- | Determine if the given list of nodes is indeed a clique,
--   and not a smaller subgraph of a clique.
isClique       :: (Graph g) => g a b -> NGroup -> Bool
isClique _  [] = False
isClique gr ns = null .
                 foldl1' intersect .
                 map ((\\ ns) . corecursive gr) $ ns

-- | Find all regular subgraphs of the given graph.
findRegular :: (Graph g) => g a b -> [[Node]]
findRegular = concat . unfoldr findRegularOf

-- | Extract the next regular subgraph of a graph.
findRegularOf :: (Graph g) => g a b -> Maybe ([[Node]], g a b)
findRegularOf g
    | isEmpty g = Nothing
    | otherwise = Just .
                  first (regularOf g . node') .
                  matchAny $ g

-- | Returns all regular subgraphs that include the given node.
regularOf      :: (Graph g) => g a b -> Node -> [[Node]]
regularOf gr n = map (n:) (alsoRegular gr crs)
    where
      crs = corecursive gr n

-- | Recursively find all regular subgraphs only containing nodes
--   in the given list.
alsoRegular          :: (Graph g) => g a b -> [Node] -> [[Node]]
alsoRegular _ []     = []
alsoRegular _ [n]    = [[n]]
alsoRegular g (n:ns) = [n] : rs ++ (alsoRegular g ns)
    where
      rs = map (n:) (alsoRegular g $ intersect crn ns)
      crn = corecursive g n

-- | Return all nodes that are co-recursive with the given node
--   (i.e. for n, find all n' such that n->n' and n'->n).
corecursive      :: (Graph g) => g a b -> Node -> [Node]
corecursive gr n = filter (elem n . suc gr) (delete n $ suc gr n)

-- | Determines if the list of nodes represents a regular subgraph.
isRegular      :: (Graph g) => g a b -> NGroup -> Bool
isRegular g ns = all allCorecursive split
    where
      -- Node + Rest of list
      split = zip ns tns'
      tns' = tail $ tails ns
      allCorecursive (n,rs) = null $ rs \\ (corecursive g n)

-- -----------------------------------------------------------------------------
{- $cycles
   Cycle detection.  Find cycles by finding all paths from a given
   node, and seeing if it reaches itself again.
 -}

-- | Find all cycles in the given graph.
cyclesIn   :: (DynGraph g) => g a b -> [LNGroup a]
cyclesIn g = map (addLabels g) (cyclesIn' g)

-- | Find all cycles in the given graph, returning just the nodes.
cyclesIn' :: (DynGraph g) => g a b -> [NGroup]
cyclesIn' = filter (not . single) -- Exclude trivial cycles, i.e. loops
            . concat . unfoldr findCycles

-- | Find all cycles in the given graph, excluding those that are also cliques.
uniqueCycles   :: (DynGraph g) => g a b -> [LNGroup a]
uniqueCycles g = map (addLabels g) (uniqueCycles' g)

-- | Find all cycles in the given graph, excluding those that are also cliques.
uniqueCycles'   :: (DynGraph g) => g a b -> [NGroup]
uniqueCycles' g = filter (not . isRegular g) (cyclesIn' g)

-- | Find all cycles containing a chosen node.
findCycles :: (DynGraph g) => g a b -> Maybe ([NGroup], g a b)
findCycles g
    | isEmpty g = Nothing
    | otherwise = Just . getCycles . matchAny $ g
    where
      getCycles (ctx,g') = (cyclesFor (ctx, g'), g')

-- | Find all cycles for the given node.
cyclesFor :: (DynGraph g) => GDecomp g a b -> [NGroup]
cyclesFor = map init .
            filter isCycle .
            pathTree .
            first Just
    where
      isCycle p = (not $ single p) && ((head p) == (last p))

-- -----------------------------------------------------------------------------

{- $chains
   A chain is a path in a graph where for each interior node, there is
   exactly one predecessor and one successor node, i.e. that part of
   the graph forms a \"straight line\".  Furthermore, the initial node
   should have only one successor, and the final node should have only
   one predecessor.  Chains are found by recursively finding the next
   successor in the chain, until either a leaf node is reached or no
   more nodes match the criteria.
-}

-- | Find all chains in the given graph.
chainsIn   :: (DynGraph g, Eq b) => g a b -> [LNGroup a]
chainsIn g = map (addLabels g)
             $ chainsIn' g

-- | Find all chains in the given graph.
chainsIn'   :: (DynGraph g, Eq b) => g a b -> [NGroup]
chainsIn' g = filter (not . single) -- Remove trivial chains
              . map (getChain g')
              $ filterNodes' isChainStart g'
    where
      -- Try to make this work on two-element cycles, undirected
      -- graphs, etc.
      g' = oneWay g

-- | Find the chain starting with the given 'Node'.
getChain     :: (Graph g) => g a b -> Node -> NGroup
getChain g n = n : (unfoldr (chainLink g) (chainNext g n))

-- | Find the next link in the chain.
chainLink :: (Graph g) => g a b -> Maybe Node
          -> Maybe (Node, Maybe Node)
chainLink _ Nothing = Nothing
chainLink g (Just n)
    | isEmpty g         = Nothing
    | not $ hasPrev g n = Nothing
    | otherwise         = Just (n, chainNext g n)

-- | Determines if the given node is the start of a chain.
isChainStart     :: (Graph g) => g a b -> Node -> Bool
isChainStart g n = (hasNext g n)
                   && case (pre g n \\ [n]) of
                        [n'] -> not $ isChainStart g n'
                        _    -> True

-- | Determine if the given node matches the chain criteria in the given
--   direction, and if so what the next node in that direction is.
chainFind         :: (Graph g) => (g a b -> Node -> NGroup)
                  -> g a b -> Node -> Maybe Node
chainFind f g n = case ((nub $ f g n) \\ [n]) of
                    [n'] -> Just n'
                    _    -> Nothing

-- | Find the next node in the chain.
chainNext :: (Graph g) => g a b -> Node -> Maybe Node
chainNext = chainFind suc

-- | Determines if this node matches the successor criteria for chains.
hasNext   :: (Graph g) => g a b -> Node -> Bool
hasNext g = isJust . chainNext g

-- | Determines if this node matches the predecessor criteria for chains.
hasPrev   :: (Graph g) => g a b -> Node -> Bool
hasPrev g = isJust . chainFind pre g

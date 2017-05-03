{- |
   Module      : Data.Graph.Analysis.Algorithms.Common
   Description : Algorithms for all graph types.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   Defines algorithms that work on both undirected and
   directed graphs.
 -}
module Data.Graph.Analysis.Algorithms.Common
    ( -- * Graph decomposition
      -- $connected
      componentsOf,
      strongComponentsOf,
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
import Data.List(unfoldr, foldl', foldl1', intersect, (\\), delete, tails, nub)
import Data.Maybe(isJust, fromJust)
import Control.Arrow(first)
import qualified Data.Map as M

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

-- | Contains the necessary data structures used by 'strongComponentsOf'.
data SCCState g a b
  = SCCState
      { sccComponents :: [g a b]
        -- ^ The components found so far.
      , sccCurrentIndex :: Int
        -- ^ The current index.
      , sccStack :: [Node]
        -- ^ The node stack.
      , sccNodeInfo :: M.Map Node (Bool, Int, Int)
        -- ^ Node information as a tuple (whether the node is on the stack, its
        -- index, and its low link).
      , sccGraph :: g a b
        -- ^ The input graph.
      }

-- | Find all strongly connected components of a graph. Implements Tarjan's
-- algorithm. Returned list is sorted in topological order.
strongComponentsOf :: (DynGraph g) => g a b -> [g a b]
strongComponentsOf g =
  sccComponents $
  foldr ( \n st ->
          let (_, i, _) = sccNodeInfo st M.! n
          in if i < 0 then findSCCFor n st else st
        )
        (mkInitSCCState g)
        (nodes g)

findSCCFor :: (DynGraph g) => Node -> SCCState g a b -> SCCState g a b
findSCCFor n st0 =
  let i = sccCurrentIndex st0
      st1 = st0 { sccCurrentIndex = i + 1
                , sccStack = (n:sccStack st0)
                , sccNodeInfo = M.insert n (True, i, i) (sccNodeInfo st0)
                }
      g = sccGraph st1
      st2 = foldr ( \m st ->
                    let st_ni = sccNodeInfo st
                        (m_on_stack, m_index, _) = st_ni M.! m
                    in if m_index < 0
                       then let st' = findSCCFor m st
                                st_ni' = sccNodeInfo st'
                                (n_on_stack', n_index', n_lowlink') =
                                  st_ni' M.! n
                                (_, _, m_lowlink) = st_ni' M.! m
                                new_n_ni = ( n_on_stack'
                                           , n_index'
                                           , min n_lowlink' m_lowlink
                                           )
                            in st' { sccNodeInfo =
                                       M.insert n new_n_ni st_ni'
                                   }
                       else if m_on_stack
                            then let (n_on_stack', n_index', n_lowlink') =
                                       st_ni M.! n
                                     new_n_ni = ( n_on_stack'
                                                , n_index'
                                                , min n_lowlink' m_index
                                                )
                                 in st { sccNodeInfo =
                                           M.insert n new_n_ni st_ni
                                       }
                            else st
                  )
                  st1
                  (suc g n)
      (_, n_index, n_lowlink) = sccNodeInfo st2 M.! n
      st3 = if n_index == n_lowlink
            then let stack = sccStack st2
                     (p0, p1) = span (/= n) stack
                     comp_ns = (head p1:p0)
                     new_stack = tail p1
                     new_ni = foldr ( \n' ni ->
                                      let (_, n_index', n_lowlink') = ni M.! n'
                                          new_n_ni = ( False
                                                     , n_index'
                                                     , n_lowlink'
                                                     )
                                      in M.insert n' new_n_ni ni
                                    )
                                    (sccNodeInfo st2)
                                    comp_ns
                     comp = nfilter (`elem` comp_ns) (sccGraph st2)
                     new_cs = (comp:sccComponents st2)
                 in st2 { sccComponents = new_cs
                        , sccStack = new_stack
                        , sccNodeInfo = new_ni
                        }
            else st2
  in st3

mkInitSCCState :: (DynGraph g) => g a b -> SCCState g a b
mkInitSCCState g =
  let ns = nodes g
  in SCCState { sccComponents = []
              , sccCurrentIndex = 0
              , sccStack = []
              , sccNodeInfo = M.fromList $ zip ns (repeat (False, -1, -1))
              , sccGraph = g
              }

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
cliquesIn    :: (DynGraph g) => g a b -> [[LNode a]]
cliquesIn gr = map (addLabels gr) (cliquesIn' gr)

-- | Finds all cliques in the graph, without including labels.
cliquesIn'    :: (DynGraph g) => g a b -> [NGroup]
cliquesIn' gr = filter (isClique gr') (findRegular gr')
    where
      gr' = mkSimple gr

-- | Determine if the given list of nodes is indeed a clique,
--   and not a smaller subgraph of a clique.
isClique       :: (Graph g) => g a b -> NGroup -> Bool
isClique _  [] = False
isClique gr ns = null .
                 foldl1' intersect .
                 map ((\\ ns) . twoCycle gr) $ ns

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
      crs = twoCycle gr n

-- | Recursively find all regular subgraphs only containing nodes
--   in the given list.
alsoRegular          :: (Graph g) => g a b -> [Node] -> [[Node]]
alsoRegular _ []     = []
alsoRegular _ [n]    = [[n]]
alsoRegular g (n:ns) = [n] : rs ++ alsoRegular g ns
    where
      rs = map (n:) (alsoRegular g $ intersect crn ns)
      crn = twoCycle g n

-- | Return all nodes that are co-recursive with the given node
--   (i.e. for n, find all n' such that n->n' and n'->n).
twoCycle      :: (Graph g) => g a b -> Node -> [Node]
twoCycle gr n = filter (elem n . suc gr) (delete n $ suc gr n)

-- | Determines if the list of nodes represents a regular subgraph.
isRegular      :: (Graph g) => g a b -> NGroup -> Bool
isRegular g ns = all allTwoCycle split
    where
      -- Node + Rest of list
      split = zip ns tns'
      tns' = tail $ tails ns
      allTwoCycle (n,rs) = null $ rs \\ twoCycle g n

-- -----------------------------------------------------------------------------
{- $cycles
   Cycle detection.  Find cycles by finding all paths from a given
   node, and seeing if it reaches itself again.
 -}

-- | Contains the necessary data structures used by 'cyclesIn'.
data CyclesInState g a b
  = CyclesInState
      { cisCycles :: [[Node]]
        -- ^ The cycles found so far, in topological order.
      , cisBlocked :: M.Map Node Bool
        -- ^ The nodes which are currently blocked.
      , cisBlockMap :: M.Map Node [Node]
        -- ^ The B set.
      , cisStack :: [Node]
        -- ^ The node stack.
      , cisS :: Maybe Node
        -- ^ The current S value.
      , cisCurrentComp :: Maybe (g a b)
        -- ^ The component currently being processed.
      , cisComponents :: [g a b]
        -- ^ The components of the input graph.
      , cisGraph :: g a b
        -- ^ The input graph.
      }

-- | Finds all cycles in a given graph using Johnson's algorithm.
--
-- See Donald B. Johnson: Finding All the Elementary Circuits of a Directed
-- Graph. SIAM Journal on Computing. Volumne 4, Nr. 1 (1975), pp. 77-84.
cyclesIn :: (DynGraph g) => g a b -> [LNGroup a]
cyclesIn g = map (addLabels g) (cyclesIn' g)

-- | Finds all cycles in a given graph using Johnson's algorithm.
--
-- See Donald B. Johnson: Finding All the Elementary Circuits of a Directed
-- Graph. SIAM Journal on Computing. Volumne 4, Nr. 1 (1975), pp. 77-84.
cyclesIn' :: (DynGraph g) => g a b -> [NGroup]
cyclesIn' g =
  cisCycles $
  foldr cyclesFor (mkInitCyclesInState g) (nodes g)

-- | Find all cycles in the given graph, excluding those that are also cliques.
uniqueCycles   :: (DynGraph g) => g a b -> [LNGroup a]
uniqueCycles g = map (addLabels g) (uniqueCycles' g)

-- | Find all cycles in the given graph, excluding those that are also cliques.
uniqueCycles'   :: (DynGraph g) => g a b -> [NGroup]
uniqueCycles' g = filter (not . isRegular g) (cyclesIn' g)

cyclesFor :: (DynGraph g) => Node -> CyclesInState g a b -> CyclesInState g a b
cyclesFor n st0 =
  let n_comp = head $
               filter (\c -> n `gelem` c) $
               cisComponents st0
  in if noNodes n_comp > 1
     then let st1 = st0 { cisS = Just n
                        , cisCurrentComp = Just n_comp
                        }
              st2 = fst $ cCircuits n st1
              g = cisGraph st2
              new_g = delNode n g
              new_comps = strongComponentsOf new_g
              st3 = st2 { cisGraph = new_g
                        , cisComponents = new_comps
                        }
          in st3
     else st0 -- Skip to next node

cCircuits :: (DynGraph g) => Node -> CyclesInState g a b ->
             (CyclesInState g a b, Bool)
cCircuits n st0 =
  let st1 = st0 { cisBlocked = M.insert n True (cisBlocked st0)
                , cisStack = (n:cisStack st0)
                }
      c = fromJust $ cisCurrentComp st1
      n_suc = suc c n
      (st2, f) =
        foldr ( \m (st, f') ->
                if m == fromJust (cisS st)
                then let new_cycle = reverse (m:cisStack st)
                         st' = st { cisCycles = (new_cycle:cisCycles st) }
                     in (st', True)
                else if not (cisBlocked st M.! m)
                     then let (st', f'') = cCircuits m st
                          in (st', f' || f'')
                     else (st, f')
              )
              (st1, False)
              n_suc
      st3 = if f
            then cUnblock n st2
            else foldr ( \m st ->
                         let bm = cisBlockMap st
                             m_blocked = bm M.! m
                             new_m_blocked = (n:m_blocked)
                         in if n `notElem` m_blocked
                            then st { cisBlockMap =
                                        M.insert m new_m_blocked bm
                                    }
                            else st
                       )
                       st2
                       n_suc
      st4 = st3 { cisStack = tail $ cisStack st3 }
  in (st4, f)

cUnblock :: (DynGraph g) => Node -> CyclesInState g a b -> CyclesInState g a b
cUnblock n st0 =
  let n_blocked = cisBlockMap st0 M.! n
      st1 = st0 { cisBlocked = M.insert n False (cisBlocked st0)
                , cisBlockMap = M.insert n [] (cisBlockMap st0)
                }
      st2 = foldr ( \m st ->
                    if cisBlocked st M.! m
                    then cUnblock m st
                    else st
                  )
                  st1
                  n_blocked
  in st2

mkInitCyclesInState :: (DynGraph g) => g a b -> CyclesInState g a b
mkInitCyclesInState g =
  let ns = nodes g
  in CyclesInState { cisCycles = []
                   , cisBlocked = M.fromList $ zip ns (repeat False)
                   , cisBlockMap = M.fromList $ zip ns (repeat [])
                   , cisStack = []
                   , cisS = Nothing
                   , cisCurrentComp = Nothing
                   , cisComponents = strongComponentsOf g
                   , cisGraph = g
                   }

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
      -- graphs, etc.  Also remove multiple edges, etc.
      g' = oneWay $ mkSimple g

-- | Find the chain starting with the given 'Node'.
getChain     :: (Graph g) => g a b -> Node -> NGroup
getChain g n = n : unfoldr (chainLink g) (chainNext g n)

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
isChainStart g n = hasNext g n
                   && case (pre g n \\ [n]) of
                        [n'] -> not $ isChainStart g n'
                        _    -> True

-- | Determine if the given node matches the chain criteria in the given
--   direction, and if so what the next node in that direction is.
chainFind         :: (Graph g) => (g a b -> Node -> NGroup)
                  -> g a b -> Node -> Maybe Node
chainFind f g n = case (nub (f g n) \\ [n]) of
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

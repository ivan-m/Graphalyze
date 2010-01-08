{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.Graph.Analysis.Internal
   Description : Internal definitions
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines various internal definitions utilised by other
   modules of the Graphalyze library.
 -}
module Data.Graph.Analysis.Internal where

import Data.Graph.Inductive.Graph

import Data.Either(partitionEithers)
import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.Set as S
import Data.Set(Set)
import Data.Maybe(fromJust)
import Control.Arrow((***))
import Control.Monad(ap)

-- -----------------------------------------------------------------------------

-- | Squaring a number.
sq   :: (Num a) => a -> a
sq x = x * x

-- | Shorthand for 'fromIntegral'
fI :: (Num a) => Int -> a
fI = fromIntegral

-- | Flip a pair.
swap       :: (a,b) -> (b,a)
swap (a,b) = (b,a)

-- | Apply the same function to both elements of a pair.
applyBoth :: (a -> b) -> (a,a) -> (b,b)
applyBoth f = f *** f

-- | Create a lookup 'Map' to determine which 'Node' has a specific label.
mkNodeMap :: (Ord a) => [LNode a] -> Map a Node
mkNodeMap = M.fromList . map swap

spreadOut :: [([a], b)] -> [(a,b)]
spreadOut = concatMap spread
  where
    spread (as, b) = map (flip (,) b) as

-- -----------------------------------------------------------------------------
-- Items re-exported in Utils (needed by Types, so defined here to
-- avoid cycles).

-- | The node number of an 'LNode'.
node :: LNode a -> Node
node = fst

-- | The label of an 'LNode'.
label :: LNode a -> a
label = snd

-- | Find all the labelled nodes in the graph that match the given predicate.
filterNodes     :: (Graph g) => (g a b -> LNode a -> Bool) -> g a b -> [LNode a]
filterNodes p g = filter (p g) (labNodes g)

-- | Find all the nodes in the graph that match the given predicate.
filterNodes'     :: (Graph g) => (g a b -> Node -> Bool) -> g a b -> [Node]
filterNodes' p g = filter (p g) (nodes g)

-- | Obtain the labels for a list of 'Node's.
--   It is assumed that each 'Node' is indeed present in the given graph.
addLabels    :: (Graph g) => g a b -> [Node] -> [LNode a]
addLabels gr = map (ap (,) (fromJust . lab gr))

-- | Obtain the labels for a 'Set' of 'Node's.
--   It is assumed that each 'Node' is indeed present in the given graph.
addLabels'    :: (Ord a, Graph g) => g a b -> Set Node -> Set (LNode a)
addLabels' gr = S.map (ap (,) (fromJust . lab gr))

-- | Obtain the labels for a list of 'Node's.
--   It is assumed that each 'Node' is indeed present in the given graph.
getLabels   :: (Graph g) => g a b -> [Node] -> [a]
getLabels gr = map label . addLabels gr

-- | Obtain the labels for a list of 'Node's.
--   It is assumed that each 'Node' is indeed present in the given graph.
getLabels'   :: (Ord a, Graph g) => g a b -> Set Node -> Set a
getLabels' gr = S.fromList -- List fusion might make this more
                           -- efficient than multiple S.map's with the
                           -- resulting internal re-organisation.
                . getLabels gr
                . S.toList

-- -----------------------------------------------------------------------------

-- | A relationship between two nodes with a label.
type Rel n e = (n, n, e)

applyNodes               :: (a -> b) -> Rel a e -> Rel b e
applyNodes f (n1, n2, e) = (f n1, f n2, e)

fromNode            :: Rel n e -> n
fromNode (n1, _, _) = n1

toNode            :: Rel n e -> n
toNode (_, n2, _) = n2

relLabel           :: Rel n e -> e
relLabel (_, _, e) = e


relsToEs              :: (Ord a) => Bool -> [LNode a] -> [Rel a e]
                         -> ([Rel a e], [LEdge e])
relsToEs isDir lns rs = (unRs, graphEdges)
    where
      -- Creating a lookup map from the label to the @Node@ value.
      nodeMap = mkNodeMap lns
      findNode v = M.lookup v nodeMap
      -- Validate a edge after looking its values up.
      validEdge e = case applyNodes findNode e of
                      (Just x, Just y, l) -> Right (x,y,l)
                      _                   -> Left e
      -- The valid edges in the graph.
      (unRs, gEdges) = partitionEithers $ map validEdge rs
      dupSwap' = if isDir
                 then id
                 else concatMap dupSwap
      dupSwap e@(x,y,l) | x == y    = [e]
                        | otherwise = [e, (y,x,l)]
      graphEdges = dupSwap' gEdges

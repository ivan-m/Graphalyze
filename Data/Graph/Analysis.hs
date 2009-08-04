{- |
   Module      : Data.Graph.Analysis
   Description : A Graph-Theoretic Analysis Library.
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   This is the root module of the /Graphalyze/ library, which aims to
   provide a way of analysing the relationships inherent in discrete
   data as a graph.

   This was written as part of my mathematics honours thesis,
   /Graph-Theoretic Analysis of the Relationships in Discrete Data/.
 -}
module Data.Graph.Analysis
    ( version,
      -- * Re-exporting other modules
      module Data.Graph.Analysis.Types,
      module Data.Graph.Analysis.Utils,
      module Data.Graph.Analysis.Algorithms,
      module Data.Graph.Analysis.Visualisation,
      module Data.Graph.Analysis.Reporting,
      module Data.Graph.Inductive.Graph,
      -- * Importing data
      ImportParams(..),
      defaultParams,
      importData,
      mergeUnused,
      mapAllNodes,
      mapNodeType,
      -- * Result analysis
      -- $analfuncts
      lengthAnalysis,
      classifyRoots,
      interiorChains,
      applyAlg
    ) where

import Data.Graph.Analysis.Utils
import Data.Graph.Analysis.Types
import Data.Graph.Analysis.Algorithms
import Data.Graph.Analysis.Visualisation
import Data.Graph.Analysis.Reporting

import Data.Graph.Inductive.Graph

import Data.Either(partitionEithers)
import Data.Maybe(mapMaybe)
import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.Set as S
import Data.Set(Set)
import Control.Arrow(second)
import Control.Monad(liftM)

-- -----------------------------------------------------------------------------

-- | The library version.
version :: String
version = "0.5"

{- |
   This represents the information that's being passed in that we want
   to analyse.  If the graph is undirected, it is better to list each
   edge once rather than both directions.
 -}
data ImportParams a = Params { -- | The discrete points.
                               dataPoints :: [a],
                               -- | The relationships between the points.
                               relationships :: [(a,a)],
                               -- | The expected roots of the graph.
                               --   If @'directed' = 'False'@, then this is ignored.
                               roots :: [a],
                               -- | 'False' if relationships are symmetric
                               --   (i.e. an undirected graph).
                               directed :: Bool
                             }

-- | Default values for 'ImportParams', with no roots and a directed graph.
defaultParams :: ImportParams a
defaultParams = Params { dataPoints    = [],
                         relationships = [],
                         roots         = [],
                         directed      = True
                       }

{- |
   Import data into a format suitable for analysis.  This function is
   /edge-safe/: if any datums are listed in the edges of
   'ImportParams' that aren't listed in the data points, then those
   edges are ignored.  Thus, no sanitation of the 'relationships' in
   @ImportParams@ is necessary.  The unused relations are stored in
   'unusedRelationships'.  Note that it is assumed that all datums in
   'roots' are also contained within 'dataPoints'.
 -}
importData        :: (Ord a) => ImportParams a -> GraphData a
importData params = GraphData { graph = dGraph
                              , wantedRoots = rootNodes
                              , directedData = isDir
                              , unusedRelationships = unRs
                              }
    where
      isDir = directed params
      -- Adding Node values to each of the data points.
      lNodes = zip [1..] (dataPoints params)
      -- The valid edges in the graph along with the unused relationships.
      (unRs, graphEdges) = relsToEs isDir lNodes (relationships params)
      -- Creating a lookup map from the label to the @Node@ value.
      nodeMap = mkNodeMap lNodes
      -- Validate a node
      validNode l = liftM (flip (,) l) $ M.lookup l nodeMap
      -- Construct the root nodes
      rootNodes = if isDir
                  then mapMaybe validNode (roots params)
                  else []
      -- Construct the graph.
      dGraph = mkGraph lNodes graphEdges

mkNodeMap :: (Ord a) => [LNode a] -> Map a Node
mkNodeMap = M.fromList . map swap

relsToEs              :: (Ord a) => Bool -> [LNode a]
                         -> [(a,a)] -> ([(a,a)], [UEdge])
relsToEs isDir lns rs = (unRs, graphEdges)
    where
      -- Creating a lookup map from the label to the @Node@ value.
      nodeMap = mkNodeMap lns
      findNode v = M.lookup v nodeMap
      -- Validate a edge after looking its values up.
      validEdge e@(v1,v2) = case (findNode v1, findNode v2) of
                              (Just x, Just y) -> Right (x,y)
                              _                -> Left e
      -- Add an empty edge label.
      addLabel (x,y) = (x,y,())
      -- The valid edges in the graph.
      (unRs, gEdges) = partitionEithers
                       $ map validEdge rs
      dupSwap' = if isDir
                 then id
                 else concatMap dupSwap
      dupSwap (x,y) | x == y    = [(x,y)]
                    | otherwise = [(x,y), (y,x)]
      graphEdges = map addLabel $ dupSwap' gEdges

-- | Merge the 'unusedRelationships' into the graph by adding the
--   appropriate nodes.
mergeUnused    :: (Ord a) => GraphData a -> GraphData a
mergeUnused gd = gd { graph = insEdges es' gr
                    , unusedRelationships = []
                    }
    where
      gr = graph gd
      unRs = unusedRelationships gd
      mkS f = S.fromList $ map f unRs
      unNs = S.toList
             . flip S.difference (knownNodes gd)
             $ S.union (mkS fst) (mkS snd)
      ns' = newNodes (length unNs) gr
      gr' = flip insNodes gr $ zip ns' unNs
      -- Should no longer contain any unused rels.
      es' = snd $ relsToEs (directedData gd)
                           (labNodes gr)
                           unRs

knownNodes :: (Ord a) => GraphData a -> Set a
knownNodes = S.fromList . map snd . labNodes . graph

-- | Apply a function to the nodes after processing.
--   This might be useful in circumstances where you want to
--   reduce the data type used to a simpler one, etc.
mapAllNodes      :: (a -> b) -> GraphData a -> GraphData b
mapAllNodes f gd = gd { graph = nmap f $ graph gd
                      , wantedRoots = map (second f) $ wantedRoots gd
                      , unusedRelationships = map (applyBoth f)
                                              $ unusedRelationships gd
                      }

-- | Apply the first function to nodes in the graph, and the second
--   function to those unknown datums in 'unusedRelationships'.
--   As a sample reason for this function, it can be used to apply a
--   two-part constructor (e.g. 'Left' and 'Right' from 'Either') to
--   the nodes such that the wanted and unwanted datums can be
--   differentiated before calling 'mergeUnused'.
mapNodeType          :: (Ord a) => (a -> b) -> (a -> b)
                        -> GraphData a -> GraphData b
mapNodeType fk fu gd = gd { graph = nmap fk $ graph gd
                          , wantedRoots = map (second fk) $ wantedRoots gd
                          , unusedRelationships = map (applyBoth f)
                                                  $ unusedRelationships gd
                          }
    where
      knownNs = knownNodes gd
      f n = if S.member n knownNs
            then fk n
            else fu n

-- -----------------------------------------------------------------------------

{- $analfuncts
   Extra functions for data analysis.
 -}

-- | Returns the mean and standard deviations of the lengths of the sublists,
--   as well all those lists more than one standard deviation longer than
--   the mean.
lengthAnalysis    :: [[a]] -> (Int,Int,[(Int,[a])])
lengthAnalysis as = (av,stdDev,as'')
    where
      as' = addLengths as
      ls = map fst as'
      (av,stdDev) = statistics' ls
      as'' = filter (\(l,_) -> l > (av+stdDev)) as'

{- |
   Compare the actual roots in the graph with those that are expected
   (i.e. those in 'wantedRoots').  Returns (in order):

   * Those roots that are expected (i.e. elements of 'wantedRoots'
     that are roots).

   * Those roots that are expected but not present (i.e. elements of
     'wantedRoots' that /aren't/ roots.

   * Unexpected roots (i.e. those roots that aren't present in
     'wantedRoots').
 -}
classifyRoots    :: (Ord a) => GraphData a -> ([LNode a], [LNode a], [LNode a])
classifyRoots gd = (areWanted, notRoots, notWanted)
    where
      wntd = S.fromList $ wantedRoots gd
      rts = S.fromList $ applyAlg rootsOf gd
      areWanted = S.toList $ S.intersection wntd rts
      notRoots  = S.toList $ S.difference wntd rts
      notWanted = S.toList $ S.difference rts wntd

-- | Only return those chains (see 'chainsIn') where the non-initial
--   nodes are /not/ expected roots.
interiorChains    :: (Eq a) => GraphData a -> [LNGroup a]
interiorChains gd = filter (not . interiorRoot) chains
    where
      chains = applyAlg chainsIn gd
      rts = wantedRoots gd
      interiorRoot = any (`elem` rts) . tail

-- | Apply an algorithm to the data to be analysed.
applyAlg   :: (AGr a -> b) -> GraphData a -> b
applyAlg f = f . graph

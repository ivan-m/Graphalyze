{- |
   Module      : Data.Graph.Analysis
   Description : A Graph-Theoretic Analysis Library.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   This is the root module of the /Graphalyze/ library, which aims to
   provide a way of analysing the relationships inherent in discrete
   data as a graph.

   The original version of this library was written as part of my
   mathematics honours thesis,
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
      importData,
      -- * Result analysis
      -- $analfuncts
      lengthAnalysis,
      classifyRoots,
      unaccessibleNodes,
      interiorChains,
      collapseAndUpdate,
      collapseAndUpdate',
      levelGraphFromRoot
    ) where

import Data.Graph.Analysis.Internal
import Data.Graph.Analysis.Utils
import Data.Graph.Analysis.Types
import Data.Graph.Analysis.Algorithms
import Data.Graph.Analysis.Visualisation
import Data.Graph.Analysis.Reporting

import Data.Graph.Inductive.Graph

import Data.List(find)
import Data.Maybe(mapMaybe, maybe)
import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.Set as S
import Data.Set(Set)
import Control.Arrow(first)

import Data.Version(showVersion)
import qualified Paths_Graphalyze as Paths(version)

-- -----------------------------------------------------------------------------

-- | The library version.
version :: String
version = showVersion Paths.version

{- |
   This represents the information that's being passed in that we want
   to analyse.  If the graph is undirected, it is better to list each
   edge once rather than both directions.
 -}
data ImportParams n e = Params { -- | The discrete points.
                                 dataPoints :: [n],
                                 -- | The relationships between the points.
                                 relationships :: [Rel n e],
                                 -- | The expected roots of the graph.
                                 --   If @'directed' = 'False'@, then this is ignored.
                                 roots :: [n],
                                 -- | 'False' if relationships are symmetric
                                 --   (i.e. an undirected graph).
                                 directed :: Bool
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
importData        :: (Ord n, Ord e) => ImportParams n e -> GraphData n e
importData params = GraphData { graph = dGraph
                              , wantedRootNodes = rootNodes
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
      validNode l = M.lookup l nodeMap
      -- Construct the root nodes
      rootNodes = if isDir
                  then mapMaybe validNode (roots params)
                  else []
      -- Construct the graph.
      dGraph = mkGraph lNodes graphEdges

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
   (i.e. those in 'wantedRootNodes').  Returns (in order):

   * Those roots that are expected (i.e. elements of 'wantedRootNodes'
     that are roots).

   * Those roots that are expected but not present (i.e. elements of
     'wantedRootNodes' that /aren't/ roots.

   * Unexpected roots (i.e. those roots that aren't present in
     'wantedRootNodes').
 -}
classifyRoots    :: GraphData n e -> (Set Node, Set Node, Set Node)
classifyRoots gd = (areWanted, notRoots, notWanted)
    where
      wntd = S.fromList $ wantedRootNodes gd
      rts = S.fromList $ applyAlg rootsOf' gd
      areWanted = S.intersection wntd rts
      notRoots  = S.difference wntd rts
      notWanted = S.difference rts wntd

-- | Find the nodes that are not reachable from the expected roots
--   (i.e. those in 'wantedRootNodes').
unaccessibleNodes    :: GraphData n e -> Set Node
unaccessibleNodes gd = allNs `S.difference` reachableNs
    where
      -- We can't use accessibleOnlyFrom' on notWanted from
      -- classifyRoots, as there might be nodes that are roots but not
      -- detectable (e.g. a loop).
      allNs = S.fromList $ applyAlg nodes  gd
      rs = S.fromList $ applyAlg rootsOf' gd
      reachableNs = applyAlg accessibleFrom' gd rs

-- | Only return those chains (see 'chainsIn') where the non-initial
--   nodes are /not/ expected roots.
interiorChains    :: (Eq n, Eq e) => GraphData n e -> [LNGroup n]
interiorChains gd = filter (not . interiorRoot) chains
    where
      chains = applyAlg chainsIn gd
      rts = wantedRoots gd
      interiorRoot = any (`elem` rts) . tail

-- | As with 'collapseAndReplace', but also update the
--   'wantedRootNodes' to contain the possibly compressed nodes.
--   Since the datums they refer to may no longer exist (as they are
--   compressed), 'unusedRelationships' is set to @[]@.
collapseAndUpdate    :: (Ord n) => [AGr n e -> [(NGroup, n)]]
                        -> GraphData n e -> GraphData n e
collapseAndUpdate fs = fst . collapseAndUpdate' fs

-- | As with 'collapseAndUpdate', but also includes a lookup 'Map'
--   from the old label to the new.
collapseAndUpdate'       :: (Ord n) => [AGr n e -> [(NGroup, n)]]
                            -> GraphData n e -> (GraphData n e, Map n n)
collapseAndUpdate' fs gd = (gd', repLookup)
  where
    gr = graph gd
    (gr', reps) = collapseAndReplace' fs gr
    lns' = mkNodeMap $ labNodes gr'
    reps' = map (first S.fromList) reps
    rs = S.fromList $ wantedRootNodes gd
    replace r = maybe r ((M.!) lns' . snd)
                $ find (S.member r . fst) reps'
    gd' = gd { graph = gr'
             , wantedRootNodes = S.toList $ S.map replace rs
             , unusedRelationships = []
             }
    nlLookup = M.fromList $ labNodes gr
    getLs = mapMaybe (flip M.lookup nlLookup)
    repLookup = M.fromList . spreadOut $ map (first getLs) reps

-- | As with 'levelGraph', but use the expected roots rather than the
--   actual roots.
levelGraphFromRoot    :: (Ord n) => GraphData n e
                         -> GraphData (GenCluster n) e
levelGraphFromRoot gd = updateGraph (levelGraphFrom (wantedRootNodes gd)) gd

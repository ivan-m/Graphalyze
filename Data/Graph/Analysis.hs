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
      manipulateNodes,
      -- * Result analysis
      -- $analfuncts
      lengthAnalysis,
      classifyRoots
    ) where

import Data.Graph.Analysis.Utils
import Data.Graph.Analysis.Types
import Data.Graph.Analysis.Algorithms
import Data.Graph.Analysis.Visualisation
import Data.Graph.Analysis.Reporting

import Data.Graph.Inductive.Graph
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Control.Arrow(second)

-- -----------------------------------------------------------------------------

-- | The library version.
version :: String
version = "0.3"

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
defaultParams = Params { dataPoints = [],
                         relationships = [],
                         roots = [],
                         directed = True
                       }

{- |
   Import data into a format suitable for analysis.  This function is
   /edge-safe/: if any datums are listed in the edges of
   'ImportParams' that aren't listed in the data points, then those
   edges are ignored.  Thus, no sanitation of the 'relationships' in
   @ImportParams@ is necessary.
 -}
importData        :: (Ord a) => ImportParams a -> GraphData a
importData params = GraphData { graph = dGraph, wantedRoots = rootNodes }
    where
      -- Adding Node values to each of the data points.
      lNodes = zip [1..] (dataPoints params)
      -- Creating a lookup map from the label to the @Node@ value.
      nodeMap = M.fromList $ map (uncurry (flip (,))) lNodes
      -- Find the Node value for the given data point.
      findNode n = M.lookup n nodeMap
      -- Validate a edge after looking its values up.
      validEdge (v1,v2) = case (findNode v1, findNode v2) of
                            (Just x, Just y) -> Just $ addLabel (x,y)
                            _                -> Nothing
      -- Add an empty edge label.
      addLabel (x,y) = (x,y,())
      -- The valid edges in the graph.
      graphEdges = catMaybes $ map validEdge (relationships params)
      -- Validate an edge
      validNode l = case (findNode l) of
                      (Just n) -> Just (n,l)
                      _        -> Nothing
      -- Construct the root nodes
      rootNodes = if (directed params)
                  then catMaybes $ map validNode (roots params)
                  else []
      -- Make the graph undirected if necessary.
      setDirection = if (directed params) then id else undir
      -- Construct the graph.
      dGraph = setDirection $ mkGraph lNodes graphEdges

-- | Apply a function to the nodes after processing.
--   This might be useful in circumstances where you want to
--   reduce the data type used to a simpler one, etc.
manipulateNodes      :: (a -> b) -> GraphData a -> GraphData b
manipulateNodes f gd = gd { graph = nmap f (graph gd)
                          , wantedRoots = map (second f) (wantedRoots gd)
                          }

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
classifyRoots    :: (Eq a) => GraphData a -> ([LNode a], [LNode a], [LNode a])
classifyRoots gd = (areWanted, notRoots, notWanted)
    where
      g = graph gd
      wntd = wantedRoots gd
      rts = rootsOf g
      areWanted = intersect wntd rts
      notRoots = wntd \\ rts
      notWanted = rts \\ wntd

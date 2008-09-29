{- |
   Module      : Data.Graph.Analysis.Visualisation
   Description : Graphviz wrapper functions
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   A wrapper module around the Haskell "Data.GraphViz" library to
   turn Graphs into basic graphs for processing by the Graphviz
   application.
 -}
module Data.Graph.Analysis.Visualisation where

import Data.Graph.Analysis.Types
import Data.Graph.Analysis.Utils
import Data.Graph.Inductive.Graph
import Data.GraphViz

-- | Turns the graph into 'DotGraph' format with the given title.
--   Nodes are labelled, edges aren't.
graphviz     :: (Graph g, Show a, Ord b) => String -> g a b -> DotGraph
graphviz t g = graphToDot g attrs nattrs eattrs
    where
      attrs = [Label t]
      nattrs (_,a) = [Label (show a)]
      eattrs _ = []

-- | Turns the graph into 'DotGraph' format with the given title.
--   Cluster the nodes based upon their 'ClusterLabel' clusters.
--   Nodes and clusters are labelled, edges aren't.
graphvizClusters :: (Graph g, Show c, ClusterLabel a c, Ord b) =>
                    String -> g a b -> DotGraph
graphvizClusters t g = clusterGraphToDot g atts assignCluster catts natts eatts
    where
      atts = [Label t]
      catts c = [Label (show c)]
      natts (_,a) = [Label (nodelabel a)]
      eatts _ = []

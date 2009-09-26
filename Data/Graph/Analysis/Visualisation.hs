{- |
   Module      : Data.Graph.Analysis.Visualisation
   Description : Graphviz wrapper functions
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   Functions to assist in visualising graphs and components of graphs.
 -}
module Data.Graph.Analysis.Visualisation
    ( -- * Graph visualisation
      -- $graphviz
      graphviz,
      graphvizClusters,
      graphvizClusters',
      assignCluster,
      noAttributes,
      -- * Showing node groupings
      -- $other
      showPath,
      showCycle,
      showNodes
    ) where

import Prelude

import Data.Graph.Analysis.Types
import Data.Graph.Analysis.Utils
import Data.Graph.Inductive.Graph
import Data.GraphViz

-- -----------------------------------------------------------------------------

{- $graphviz
   Simple wrappers around the Haskell "Data.GraphViz" library to turn
   'GraphData's into basic 'DotGraph's for processing by the GraphViz
   suite of applications.
-}

-- | Convert the 'GraphData' into 'DotGraph' format with the given
--   'Attribute's.
graphviz :: GraphData n e -> [GlobalAttributes]
            -> (LNode n -> Attributes)
            -> (LEdge e -> Attributes) -> DotGraph Node
graphviz = applyDirAlg graphToDot

-- | Convert the clustered 'GraphData' into 'DotGraph' format with the
--   given 'Attribute's.  Cluster the nodes based upon their
--   'ClusterLabel' clusters.
graphvizClusters :: (ClusterLabel cl) => GraphData cl e -> [GlobalAttributes]
                    -> (Cluster cl -> [GlobalAttributes])
                    -> (LNode cl -> Attributes)
                    -> (LEdge e -> Attributes) -> DotGraph Node
graphvizClusters g gas = graphvizClusters' g gas assignCluster clusterID

-- | Convert the 'GraphData' into a clustered 'DotGraph' format using
--   the given clustering function and with the given 'Attribute's.
graphvizClusters' :: (Ord c) => GraphData n e -> [GlobalAttributes]
                     -> (LNode n -> NodeCluster c n)
                     -> (c -> Maybe GraphID)
                     -> (c -> [GlobalAttributes]) -> (LNode n -> Attributes)
                     -> (LEdge e -> Attributes) -> DotGraph Node
graphvizClusters' = applyDirAlg clusterGraphToDot

-- | A function to convert an 'LNode' to the required 'NodeCluster'
--   for use with the GraphViz library.
assignCluster          :: (ClusterLabel cl) => LNode cl -> NodeCluster (Cluster cl) cl
assignCluster nl@(_,a) = C (cluster a) (N nl)

-- | Used to state that GraphViz should use the default 'Attribute's
--   for the given value.
noAttributes :: a -> Attributes
noAttributes = const []

-- -----------------------------------------------------------------------------

{- $other
   Other visualisations.  These are mainly replacements for
   the @show@ function.
 -}

-- | Print a path, with \"->\" between each element.
showPath     :: (Show a) => LNGroup a -> String
showPath []  = ""
showPath lns = blockPrint' (l:ls')
    where
      -- Can't use blockPrintWith above, as it only takes a per-row spacer.
      (l:ls) = map (show . label) lns
      ls' = map ("-> "++) ls

-- | Print a cycle: copies the first node to the end of the list,
--   and then calls 'showPath'.
showCycle            ::(Show a) => LNGroup a -> String
showCycle []         = ""
showCycle lns@(ln:_) = showPath (lns ++ [ln])

-- | Show a group of nodes, with no implicit ordering.
showNodes     :: (Show a) => LNGroup a -> String
showNodes []  = ""
showNodes lns = blockPrint' . addCommas
                $ map (show . label) lns
    where
      addCommas []     = []
      addCommas [l]    = [l]
      addCommas (l:ls) = (l ++ ", ") : addCommas ls

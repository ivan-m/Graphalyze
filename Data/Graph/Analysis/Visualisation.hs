{- |
   Module      : Data.Graph.Analysis.Visualisation
   Description : Graphviz wrapper functions
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   Functions to assist in visualising graphs and components of graphs.
 -}
module Data.Graph.Analysis.Visualisation
    ( -- * Graph visualisation
      -- $graphviz
      graphviz,
      graphvizClusters,
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

import System.IO
import Control.Concurrent
import Control.Exception

-- -----------------------------------------------------------------------------

{- $graphviz
   Simple wrappers around the Haskell "Data.GraphViz" library to
   turn 'Graph's into basic 'DotGraph's for processing by the Graphviz
   application.
-}

-- | Turns the graph into 'DotGraph' format with the given title and graph
--   attributes.  Nodes are labelled, edges aren't.
graphviz        :: (Graph g, Show a, Ord b) => String -> g a b -> [Attribute]
                -> DotGraph
graphviz t g as = graphToDot g attrs nattrs eattrs
    where
      attrs = Label (StrLabel t) : as
      nattrs (_,a) = [Label . StrLabel $ show a]
      eattrs _ = []

-- | Turns the graph into 'DotGraph' format with the given title and graph
--   attributes.  Cluster the nodes based upon their 'ClusterLabel' clusters.
--   Nodes and clusters are labelled, edges aren't.
graphvizClusters :: (Graph g, Show c, ClusterLabel a c, Ord b) =>
                    String -> g a b -> [Attribute] -> DotGraph
graphvizClusters t g as = clusterGraphToDot g atts assignCluster cas nas eas
    where
      atts = Label (StrLabel t) : as
      cas c = [Label . StrLabel $ show c]
      nas (_,a) = [Label . StrLabel $ nodelabel a]
      eas _ = []

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

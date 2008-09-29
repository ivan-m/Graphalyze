{-# LANGUAGE MultiParamTypeClasses
            , FunctionalDependencies
 #-}

{- |
   Module      : Data.Graph.Analysis.Types
   Description : Graphalyze Types and Classes
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines the various types and classes utilised
   by the Graphalyze library.
 -}
module Data.Graph.Analysis.Types
    ( -- * Graph specialization
      GraphData(..),
      Gr,
      AGr,
      NGroup,
      LNGroup,
      -- * Graph Label classes
      ClusterLabel(..),
      GenCluster(..),
      PosLabel(..)
    ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

-- -----------------------------------------------------------------------------

{- |
   By default, the Graphalyze library works on graphs with no edge labels.
   As such, these types provide useful aliases for the default FGL types.
   Most of the algorithms, however, work on arbitrary graph types.
 -}

-- | Represents information about the graph being analysed.
data GraphData a = GraphData { -- | We use a graph type with no edge labels.
                               graph :: AGr a,
                                -- | The expected roots in the graph.
                               wantedRoots :: LNGroup a
                             }
                   deriving (Show)

-- | We use a basic tree-based graph by default.
type AGr a = Gr a ()

-- | A grouping of 'Node's.
type NGroup = [Node]

-- | A grouping of 'LNode's.
type LNGroup a = [LNode a]

-- -----------------------------------------------------------------------------

-- | These types and classes represent useful label types.

-- | The class of outputs of a clustering algorithm.
--   This class is mainly used for visualization purposes,
--   with the 'Ord' instance required for grouping.
--   Instances of this class are intended for use as
--   the label type of graphs.
class (Ord c) => ClusterLabel a c | a -> c where
    -- | The cluster the node label belongs in.
    cluster   :: a -> c
    -- | The printed form of the actual label.
    nodelabel :: a -> String

-- | A generic cluster-label type.
data GenCluster a = GC Int a

instance (Show a) => ClusterLabel (GenCluster a) Int where
    cluster (GC c _) = c
    nodelabel (GC _ l) = show l

-- | Label type for storing node positions.  Note that this isn't an instance of
--   'ClusterLabel' since there's no clear indication on which cluster a node
--   belongs to at this stage.
data PosLabel a = PLabel { xPos   :: Int
                         , yPos   :: Int
                         , pnode  :: Node
                         , plabel :: a
                         }
                  deriving (Eq, Show)

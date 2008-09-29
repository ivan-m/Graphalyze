{- |
   Module      : Data.Graph.Analysis.Algorithms
   Description : Graph analysis algorithms
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module exports all the algorithms found in the
   @Data.Graph.Analysis.Algorithms.*@ modules.
 -}
module Data.Graph.Analysis.Algorithms
    ( module Data.Graph.Analysis.Algorithms.Common,
      module Data.Graph.Analysis.Algorithms.Directed,
      module Data.Graph.Analysis.Algorithms.Clustering,
      applyAlg
    ) where

import Data.Graph.Analysis.Types
import Data.Graph.Analysis.Algorithms.Common
import Data.Graph.Analysis.Algorithms.Directed
import Data.Graph.Analysis.Algorithms.Clustering

{- |
   For algorithms that return a group of nodes, there are typically
   two different forms: the standard form (e.g. @cliquesIn@) will
   return a list of @LNode@s, whilst the primed version
   (e.g. @cliquesIn'@) will return a list of @Node@s.
 -}

-- | Apply an algorithm to the data to be analysed.
applyAlg   :: (AGr a -> b) -> GraphData a -> b
applyAlg f = f . graph


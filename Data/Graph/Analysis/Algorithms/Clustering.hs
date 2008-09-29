{-# LANGUAGE MultiParamTypeClasses #-}

{- |
   Module      : Data.Graph.Analysis.Algorithms.Clustering
   Description : Clustering and grouping algorithms.
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   Clustering and grouping algorithms that are graph-invariant and require
   no user intervention.
 -}
module Data.Graph.Analysis.Algorithms.Clustering
    ( -- * Clustering Algorithms
      -- ** Non-deterministic algorithms
      Whispering,
      chineseWhispers,
      -- ** Spatial Algorithms
      relativeNeighbourhood,
      -- * Graph Collapsing
      CNodes(..),
      collapseGraph
    ) where

import Data.Graph.Analysis.Types
import Data.Graph.Analysis.Utils
import Data.Graph.Analysis.Algorithms.Common
import Data.Graph.Analysis.Algorithms.Directed(rootsOf')

import Data.Graph.Inductive.Graph
import Data.GraphViz.Attributes

import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Set.BKTree as BK
import Data.Set.BKTree(BKTree, Metric(..))
import Control.Arrow
import System.Random

-- -----------------------------------------------------------------------------

{- |
   The Chinese Whispering Algorithm.
   This is an adaptation of the algorithm described in:

   Biemann, C. (2006): Chinese Whispers - an Efficient Graph Clustering
   Algorithm and its Application to Natural Language Processing Problems.
   Proceedings of the HLT-NAACL-06 Workshops on Textgraphs-06, New York, USA
   <http://wortschatz.uni-leipzig.de/~cbiemann/pub/2006/BiemannTextGraph06.pdf>

   The adaptations to this algorithm are as follows:
     * Ignore any edge weightings that may exist, as we can't depend on them
       (also, we want the algorithm to be dependent solely upon the
        /structure/ of the graph, not what it contains).
     * Increase the weighting of those nodes present in interesting structures,
       such as loops and root nodes.  This is to try and ensure that these nodes
       end up in the same cluster.

   Simplistically, the way it works is this:
     1) Every node is assigned into its own unique cluster.
     2) For each iteration, sort the nodes into each order.  For each node,
        it joins the most popular cluster in its neighbourhood
        (where popularity is defined by the sum of the weightings).
     3) Repeat step 2) until a fixed point is reached.
   Note that this algorithm is non-deterministic, and that for some graphs
   no fixed point may be reached (and the algorithm may oscillate between
   a few different graph clusterings).
-}

-- | An instance of 'ClusterLabel' used for the Chinese Whispers algorithm.
data Whispering a = W { name  :: a      -- ^ The original label.
                      , whisp :: Int    -- ^ The current cluster this node is in.
                      , coeff :: Double -- ^ The node's weighting.
                      } deriving (Show,Eq)

instance (Show a) => ClusterLabel (Whispering a) Int where
    cluster   = whisp
    nodelabel = show . name

-- | The actual Chinese Whispers algorithm.
chineseWhispers      :: (RandomGen g, Eq a, Eq b, DynGraph gr) => g -> gr a b
                     -> gr (Whispering a) b
chineseWhispers g gr = fst $ fixPointBy eq whispering (gr',g)
    where
      eq = equal `on` fst
      ns = nodes gr
      whispering (gr'',g') = foldl' whisperNode (gr'',g'') ns'
          where
            -- Shuffle the nodes to ensure the order of choosing a new
            -- cluster is random.
            (ns',g'') = shuffle g' ns
      gr' = addWhispers gr

-- | Choose a new cluster for the given @Node@.  Note that this updates
--   the graph each time a new cluster value is chosen.
whisperNode          :: (RandomGen g, DynGraph gr) => (gr (Whispering a) b,g)
                     -> Node -> (gr (Whispering a) b,g)
whisperNode (gr,g) n = (c' & gr',g')
    where
      (Just c,gr') = match n gr
      (g',c') = whisper gr g c

-- | Choose a new cluster for the given @Context@.
whisper :: (RandomGen g, Graph gr) => gr (Whispering a) b -> g
        -> Context (Whispering a) b -> (g,Context (Whispering a) b)
whisper gr g (p,n,al,s) = (g',(p,n,al {whisp = w'},s))
    where
      (w',g') = case (neighbors gr n) of
                  [] -> (whisp al,g)
                  -- Add this current node to the list of neighbours to add
                  -- extra weighting, as it seems to give better results.
                  ns -> chooseWhisper g (addLabels gr (n:ns))

-- | Choose which cluster to pick by taking the one with maximum sum of
--   weightings.  If more than one has the same maximum, choose one
--   randomly.
chooseWhisper       :: (RandomGen g) => g -> [LNode (Whispering a)]
                    -> (Int,g)
chooseWhisper g lns = pick maxWspWgts
    where
      -- This isn't the most efficient method of choosing a random list element,
      -- but the graph is assumed to be relatively sparse and thus ns should
      -- be relatively short.
      pick ns = first (ns!!) $ randomR (0,length ns - 1) g
      whispWgts = map (second sumWgts) . groupElems whisp $ map label lns
      maxWspWgts = map fst . snd . head $ groupElems (negate . snd) whispWgts
      sumWgts = sum . map coeff

-- | Convert the graph into a form suitable for the Chinese Whispers algorithm.
addWhispers   :: (DynGraph gr) => gr a b -> gr (Whispering a) b
addWhispers g = gmap augment g
    where
      augment (p,n,l,s) = (p,n,W { name  = l
                                 , whisp = n
                                 , coeff = coefFor n
                                 },s)
      -- Note that cliques are also cycles...
      cliques = Set.fromList . concat $ cliquesIn' g
      cycles = Set.fromList . concat $ cyclesIn' g
      roots = Set.fromList $ rootsOf' g
      -- Give more emphasis to interesting parts of the graph.
      coefFor n
          | Set.member n roots   = 3
          | Set.member n cycles  = 2
          | otherwise            = 1


{-

Originally used for the clustering coefficient, didn't seem to give good
results.
http://en.wikipedia.org/wiki/Clustering_coefficient

clusteringCoef     :: (Graph gr) => gr a b -> Node -> Double
clusteringCoef g n = if (liftM2 (||) isNaN isInfinite $ coef)
                     then 0
                     else coef
    where
      d = fromIntegral $ deg g n
      coef = (fromIntegral nes) / (k*(k - 1))
      ns = (neighbors g n)
      k = fromIntegral $ length ns
      nes = length $ concatMap (union ns . neighbors g) ns
-}

-- -----------------------------------------------------------------------------

{- |
   This implements the algorithm called CLUSTER, from the paper:

   Bandyopadhyay, S. (2003): An automatic shape independent clustering
   technique.  Pattern Recognition, vol. 37, pp. 33-45.

   Simplistically, it defines clusters as groups of nodes that are
   spatially located closer to each other than to nodes in
   other clusters.  It utilises the concept of a /Relative
   Neighbour Graph/ [RNG] to determine the spatial structure of a set
   of two-dimensional data points.

   The adaptations to this algorithm are as follows:
     * Due to the limitations of the BKTree data structure, we utilise a
       /fuzzy/ distance function defined as the ceiling of the standard
       Euclidian distance.
     * We utilise 'toPosGraph' to get the spatial locations.  As such,
       these locations may not be optimal, especially for smaller graphs.
     * The actual algorithm is applied to each connected component of
       the graph.  The actual paper is unclear what to do in this
       scenario, but Graphviz may locate nodes from separate
       components together, despite them not being related.

   The algorithm is renamed 'relativeNeighbourhood'.  Experimentally, it
   seems to work better with larger graphs (i.e. more nodes), since
   then Graphviz makes the apparent clusters more obvious.
-}

-- | The renamed CLUSTER algorithm.  Attempts to cluster a graph by using
--   the spatial locations used by Graphviz.
relativeNeighbourhood   :: (DynGraph gr, Eq a, Ord b) => gr a b
                        -> gr (GenCluster a) b
relativeNeighbourhood g = setCluster cMap g
    where
      cMap = createLookup . concatMap rn $ componentsOf g
      rn g' = nbrCluster rng
          where
            rng :: Gr () Int
            rng = makeRNG $ getPositions g'

-- | We take the ceiling of the Euclidian distance function to use as our
--   metric function.
instance (Eq a) => Metric (PosLabel a) where
    distance = (ceiling . ) . euclidian

-- | The Euclidian distance function.
euclidian       :: PosLabel a -> PosLabel a -> Double
euclidian n1 n2 = sqrt . fI $ (posBy xPos) + (posBy yPos)
    where
      posBy p = sq $ (p n1) - (p n2)

-- | Converts the positional labels into an RNG.
makeRNG    :: (Eq a, Graph gr) => [PosLabel a] -> gr () Int
makeRNG ls = mkGraph ns es
    where
      ns = map (\l -> (pnode l,())) ls
      tree = BK.fromList ls
      tls = tails ls
      es = [ (pnode l1,pnode l2,distance l1 l2)
                 | (l1:ls') <- tls
                 , l2 <- ls'
                 , areRelative tree l1 l2 ]

-- | Determines if the two given nodes should be connected in the RNG.
--   Nodes are connected if there is no node that is closer to both of them.
areRelative         :: (Metric a) => BKTree a -> a -> a -> Bool
areRelative t l1 l2 = null lune
    where
      d = distance l1 l2
      -- Find all nodes distance <= d away from the given node.
      -- Note that n is distance 0 <= d away from n, so we need to
      -- remove it from the list of results.
      rgnFor l = delete l $ BK.elemsDistance d l t
      -- The nodes that are between the two given nodes.
      lune = intersect (rgnFor l1) (rgnFor l2)

-- | Performs the actual clustering algorithm on the RNG.
nbrCluster   :: (DynGraph gr) => gr a Int -> [[Node]]
nbrCluster g
    | numNodes == 1 = [ns] -- Can't split up a single node.
    | eMax < 2*eMin = [ns] -- The inter-cluster relative neighbours
                           -- are too close too each other.
    | null thrs     = [ns] -- No threshold value available.
    | single cg'    = [ns] -- No edges meet the threshold deletion
                           -- criteria.
    | nCgs > sNum   = [ns] -- Over-fragmentation of the graph.
    | otherwise     = concatMap nbrCluster cg'
    where
      ns = nodes g
      numNodes = noNodes g
      sNum = floor . sqrt $ fI numNodes
      les = labEdges g
      (es,eMin,eMax) = sortMinMax $ map eLabel les
      es' = zip es (tail es)
      sub = uncurry subtract
      -- First order differences.
      -- We don't care about the list, just what the min and max diffs are.
      (_,dfMin,dfMax) = sortMinMax $ map sub es'
      -- We are going to do >= tests on t, but using Int values, so
      -- take the ceiling.
      t = ceiling $ ((fI dfMin) + (fI dfMax))/2
      -- Edges that meet the threshold criteria.
      thrs = filter (\ejs@(ej,_) -> (ej >= 2*eMin) && (sub ejs >= t)) es'
      -- Take the first edges that meets the threshold criteria.
      thresh = fst $ head thrs
      -- Edges that meet the threshold deletion criteria.
      rEs = map edge $ filter ((>= thresh) . eLabel) les
      g' = delEdges rEs g
      -- Each of these will also be an RNG
      cg' = componentsOf g'
      nCgs = length cg'

-- -----------------------------------------------------------------------------

{- |
   Collapse the /interesting/ parts of a graph down to try and show a
   compressed overview of the whole graph.  Note that this doesn't
   work too well on undirected graphs, since

   It may be possible to extend this to a clustering algorithm by
   collapsing low density regions into high density regions.
 -}

-- | A collapsed node contains a list of nodes that it represents.
data CNodes a = CN [LNode a]

-- | This definition of 'show' is written so as to make the shapes of the
--   nodes in Graphviz roughly circular, rather than one long ellipse.
instance (Show a) => Show (CNodes a) where
    -- Print the labels in a roughly square shape.
    show (CN lns) = blockPrint $ map label lns

collapseGraph   :: (DynGraph gr, Eq b) => gr a b -> gr (CNodes a) b
collapseGraph g = foldl' (flip collapseAllBy) cg interestingParts
    where
      cg = makeCollapsible g
      interestingParts = [cliquesIn', cyclesIn', chainsIn']

-- | Allow the graph to be collapsed.
makeCollapsible :: (DynGraph gr) => gr a b -> gr (CNodes a) b
makeCollapsible = nlmap (CN . return)

-- | Collapse the two given nodes into one node.
collapse         :: (DynGraph gr) => gr (CNodes a) b -> Node -> Node
                 -> gr (CNodes a) b
collapse g n1 n2 = if (n1 == n2)
                   then g
                   else c' & g''
    where
      (Just c1, g') = match n1 g
      (Just c2, g'') = match n2 g'
      -- The new edges.
      nbrBy f = map (\(a,b) -> (b,a))
                -- not sure if this should be included: . nub
                . filter (\(n,_) -> notElem n [n1,n2])
                $ (f c1 ++ f c2)
      p = nbrBy lpre'
      s = nbrBy lsuc'
      (CN l1) = lab' c1
      (CN l2) = lab' c2
      c' = (p,n1,CN (l1++l2),s)

-- | Collapse the list of nodes down to one node.
collapseAll      :: (DynGraph gr) => gr (CNodes a) b -> [Node]
                 -> gr (CNodes a) b
collapseAll g []  = g
collapseAll g [_]    = g
collapseAll g (n:ns) = foldl' collapser g ns
    where
      collapser g' = collapse g' n

-- | Collapse the results of the given function.
collapseBy     :: (DynGraph gr) => (gr (CNodes a) b -> [Node])
               -> gr (CNodes a) b -> gr (CNodes a) b
collapseBy f g = collapseAll g (f g)

-- | Collapse all results of the given function.
collapseAllBy     :: (DynGraph gr) => (gr (CNodes a) b -> [[Node]])
                  -> gr (CNodes a) b -> gr (CNodes a) b
collapseAllBy f g = case (filter (not . single) $ f g) of
                      []     -> g
                             -- We re-evaluate the function in case
                             -- the original results used nodes that
                             -- have been collapsed down.
                      (ns:_) -> collapseAllBy f (collapseAll g ns)

-- | Collapse all cliques down to single nodes.
collapseCliques :: (DynGraph gr) => gr (CNodes a) b -> gr (CNodes a) b
collapseCliques = collapseAllBy cliquesIn'

-- | Collapse all cycles down into single nodes.
collapseCycles :: (DynGraph gr) => gr (CNodes a) b -> gr (CNodes a) b
collapseCycles = collapseAllBy cyclesIn'

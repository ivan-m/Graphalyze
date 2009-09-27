{- |
   Module      : Data.Graph.Analysis.Algorithms.Clustering
   Description : Clustering and grouping algorithms.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   Clustering and grouping algorithms that are graph-invariant and require
   no user intervention.
 -}
module Data.Graph.Analysis.Algorithms.Clustering
    ( -- * Clustering Algorithms
      -- ** Non-deterministic algorithms
      -- $chinesewhispers
      chineseWhispers,
      -- ** Spatial Algorithms
      -- $relneighbours
      relativeNeighbourhood,
      -- * Graph Collapsing
      -- $collapsing
      CNodes,
      collapseGraph,
      collapseGraphBy,
      collapseGraphBy',
      trivialCollapse,
    ) where

import Data.Graph.Analysis.Internal
import Data.Graph.Analysis.Types
import Data.Graph.Analysis.Utils
import Data.Graph.Analysis.Algorithms.Common

import Data.Graph.Inductive.Graph

import Data.List(foldl', tails, delete, intersect)
import Data.Function(on)
import qualified Data.Set.BKTree as BK
import Data.Set.BKTree(BKTree, Metric(..))
import Control.Arrow(first, second)
import System.Random(RandomGen, randomR)

-- -----------------------------------------------------------------------------

{- $chinesewhispers
   The Chinese Whispers Algorithm.
   This is an adaptation of the algorithm described in:

   Biemann, C. (2006): Chinese Whispers - an Efficient Graph Clustering
   Algorithm and its Application to Natural Language Processing Problems.
   Proceedings of the HLT-NAACL-06 Workshops on Textgraphs-06, New York, USA
   <http://wortschatz.uni-leipzig.de/~cbiemann/pub/2006/BiemannTextGraph06.pdf>

   The adaptations to this algorithm are as follows:

     * Ignore any edge weightings that may exist, as we can't depend on them
       (also, we want the algorithm to be dependent solely upon the
        /structure/ of the graph, not what it contains).

     * Explicitly shuffle the node order for each iteration.

   Simplistically, the way it works is this:

     1. Every node is assigned into its own unique cluster.

     2. Sort the nodes into some random order.  Each node joins the
        most popular cluster in its neighbourhood (where popularity
        is defined as the sum of the node weightings in that cluster).

     3. Repeat step 2. until a fixed point is reached.

   Note that this algorithm is non-deterministic, and that for some graphs
   no fixed point may be reached (and the algorithm may oscillate between
   a few different graph clusterings).

   Chinese Whispers is @O(number of edges)@.
-}

-- | The actual Chinese Whispers algorithm.
chineseWhispers      :: (RandomGen g, Eq a, Eq b, DynGraph gr) => g -> gr a b
                     -> gr (GenCluster a) b
chineseWhispers g gr = reCluster . fst $ fixPointBy eq whispering (gr',g)
    where
      eq = equal `on` fst
      ns = nodes gr
      whispering (gr'',g') = foldl' whisperNode (gr'',g'') ns'
          where
            -- Shuffle the nodes to ensure the order of choosing a new
            -- cluster is random.
            (ns',g'') = shuffle g' ns
      gr' = addWhispers gr

-- | Choose a new cluster for the given 'Node'.  Note that this updates
--   the graph each time a new cluster value is chosen.
whisperNode          :: (RandomGen g, DynGraph gr) => (gr (GenCluster a) b,g)
                     -> Node -> (gr (GenCluster a) b,g)
whisperNode (gr,g) n = (c' & gr',g')
    where
      (Just c,gr') = match n gr
      (g',c') = whisper gr g c

-- | Choose a new cluster for the given @Context@.
whisper :: (RandomGen g, Graph gr) => gr (GenCluster a) b -> g
        -> Context (GenCluster a) b -> (g,Context (GenCluster a) b)
whisper gr g (p,n,al,s) = (g',(p,n,al { clust = w' },s))
    where
      (w',g') = case (neighbors gr n) of
                  [] -> (clust al,g)
                  -- Add this current node to the list of neighbours to add
                  -- extra weighting, as it seems to give better results.
                  ns -> chooseWhisper g (addLabels gr (n:ns))

-- | Choose which cluster to pick by taking the one with maximum number of
--   nodes.  If more than one has the same maximum, choose one
--   randomly.
chooseWhisper       :: (RandomGen g) => g -> [LNode (GenCluster a)]
                    -> (Int,g)
chooseWhisper g lns = pick maxWsps
    where
      -- This isn't the most efficient method of choosing a random list element,
      -- but the graph is assumed to be relatively sparse and thus ns should
      -- be relatively short.
      pick ns = first (ns!!) $ randomR (0,length ns - 1) g
      whisps = map (second length) . groupElems clust $ map label lns
      maxWsps = map fst . snd . head $ groupElems (negate . snd) whisps

-- | Convert the graph into a form suitable for the Chinese Whispers algorithm.
addWhispers   :: (DynGraph gr) => gr a b -> gr (GenCluster a) b
addWhispers g = gmap augment g
    where
      augment (p,n,l,s) = (p,n, GC { clust = n, nLbl = l }, s)

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

{- $relneighbours
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
       these locations may not be optimal, especially for smaller
       graphs.

     * The actual algorithm is applied to each connected component of
       the graph.  The actual paper is unclear what to do in this
       scenario, but Graphviz may locate nodes from separate
       components together, despite them not being related.


   The algorithm is renamed 'relativeNeighbourhood'.  Experimentally, it
   seems to work better with larger graphs (i.e. more nodes), since
   then Graphviz makes the apparent clusters more obvious.  The actual
   algorithm is @O(n^2)@, where /n/ is the number of 'Node's in the graph.
-}

-- | The renamed CLUSTER algorithm.  Attempts to cluster a graph by using
--   the spatial locations used by Graphviz.
relativeNeighbourhood       :: (DynGraph gr, Eq a, Ord b) => Bool -> gr a b
                               -> gr (GenCluster a) b
relativeNeighbourhood dir g = setCluster cMap g
    where
      cMap = createLookup $ rn g
      rn g' = nbrCluster rng
          where
            rng :: AGr () Int
            rng = makeRNG $ getPositions dir g'

-- | We take the ceiling of the Euclidian distance function to use as our
--   metric function.
instance (Eq a) => Metric (PosLabel a) where
    distance = (ceiling . ) . euclidian
-- Note that this throws an orphan instance warning.

-- | The Euclidian distance function.
euclidian       :: PosLabel a -> PosLabel a -> Double
euclidian n1 n2 = sqrt . fI $ posBy xPos + posBy yPos
    where
      posBy p = sq $ p n1 - p n2

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
nbrCluster   :: (DynGraph gr) => gr a Int -> [NGroup]
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
      sNum = floor (sqrt $ fI numNodes :: Double)
      les = labEdges g
      (es,eMin,eMax) = sortMinMax $ map eLabel les
      es' = zip es (tail es)
      sub = uncurry subtract
      -- First order differences.
      -- We don't care about the list, just what the min and max diffs are.
      (_,dfMin,dfMax) = sortMinMax $ map sub es'
      -- We are going to do >= tests on t, but using Int values, so
      -- take the ceiling.
      t = ceiling ((fI dfMin + fI dfMax)/2 :: Double)
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

{- $collapsing
   Collapse the parts of a graph down to try and show a compressed
   overview of the whole graph.

   It may be possible to extend this to a clustering algorithm by
   collapsing low density regions into high density regions.
 -}

-- | A collapsed node contains a list of nodes that it represents.
type CNodes a = [a]

-- | Collapse the cliques, cycles and chains in the graph down.  Note
--   that this doesn't work too well on undirected graphs, since every
--   pair of nodes forms a K_2 subgraph.
collapseGraph :: (DynGraph gr, Eq b) => gr a b -> gr (CNodes a) b
collapseGraph = collapseGraphBy interestingParts
    where
      interestingParts = [cliquesIn', cyclesIn', chainsIn']

-- | Use the given functions to determine which nodes to collapse.
collapseGraphBy    :: (DynGraph gr) => [gr (CNodes a) b -> [NGroup]]
                      -> gr a b -> gr (CNodes a) b
collapseGraphBy fs = collapseGr fs'
    where
      fs' = map (map (flip (,) Nothing) .) fs

-- | Use the given functions to determine which nodes to collapse,
--   with a new label to represent the collapsed nodes.
collapseGraphBy'    :: (DynGraph gr) => [gr (CNodes a) b -> [(NGroup, a)]]
                       -> gr a b -> gr a b
collapseGraphBy' fs = unCollapse . collapseGr fs'
    where
      fs' = map (map (second Just) .) fs

-- | Collapse the graph.
collapseGr      :: (DynGraph gr) => [gr (CNodes a) b -> [(NGroup, Maybe a)]]
                   -> gr a b -> gr (CNodes a) b
collapseGr fs g = foldl' collapseAllBy (makeCollapsible g) fs

-- | Return @'True'@ if the collapsed graph is either a singleton node
--   or else isomorphic to the original graph (i.e. not collapsed at all).
trivialCollapse    :: (Graph gr) => gr (CNodes a) b -> Bool
trivialCollapse cg = allCollapsed || notCollapsed
    where
      allCollapsed = single lns || null lns
      notCollapsed = all single lns
      lns = labels cg

-- | Allow the graph to be collapsed.
makeCollapsible :: (DynGraph gr) => gr a b -> gr (CNodes a) b
makeCollapsible = nmap return

unCollapse :: (DynGraph gr) => gr (CNodes a) b -> gr a b
unCollapse = nmap head

-- | Collapse the two given nodes into one node.
collapse         :: (DynGraph gr) => gr (CNodes a) b -> Node -> Node
                 -> gr (CNodes a) b
collapse g n1 n2 = if n1 == n2
                   then g
                   else c' & g''
    where
      (Just c1, g') = match n1 g
      (Just c2, g'') = match n2 g'
      -- The new edges.
      nbrBy f = map (\(a,b) -> (b,a))
                . filter (\(n,_) -> notElem n [n1,n2])
                $ (f c1 ++ f c2)
      p = nbrBy lpre'
      s = nbrBy lsuc'
      l1 = lab' c1
      l2 = lab' c2
      c' = (p,n1,l1++l2,s)

-- | Collapse the list of nodes down to one node.
collapseAll               :: (DynGraph gr) => gr (CNodes a) b
                             -> (NGroup, Maybe a)
                             -> gr (CNodes a) b
collapseAll g ([],_)      = g -- These two cases
collapseAll g ([_],_)     = g -- shouldn't occur.
collapseAll g ((n:ns),ma) = adj $ foldl' collapser g ns
    where
      adj = maybe id (adjustLabel n) ma
      collapser g' = collapse g' n

adjustLabel       :: (DynGraph gr) => Node -> a
                     -> gr (CNodes a) b -> gr (CNodes a) b
adjustLabel n a g = c & g'
    where
      (Just (p,_,_,s), g') = match n g
      c = (p,n,[a],s)

-- | Collapse all results of the given function.
collapseAllBy     :: (DynGraph gr) => gr (CNodes a) b
                     -> (gr (CNodes a) b -> [(NGroup, Maybe a)])
                     -> gr (CNodes a) b
collapseAllBy g f = case (filter (not . single . fst) $ f g) of
                      []     -> g
                             -- We re-evaluate the function in case
                             -- the original results used nodes that
                             -- have been collapsed down.
                      (nsr:_) -> collapseAllBy (collapseAll g nsr) f

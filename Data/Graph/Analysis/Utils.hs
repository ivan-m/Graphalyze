{-# LANGUAGE  OverlappingInstances
            , UndecidableInstances
            , TypeSynonymInstances
            , FlexibleInstances
 #-}

{- |
   Module      : Data.Graph.Analysis.Utils
   Description : Utility functions
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines various utility functions used throughout.
 -}
module Data.Graph.Analysis.Utils
    ( -- * Graph functions
      -- ** Data extraction
      node,
      label,
      edge,
      eLabel,
      addLabels,
      filterNodes,
      filterNodes',
      -- ** Graph manipulation
      pathValues,
      undir,
      oneWay,
      nlmap,
      -- ** Graph layout
      -- | These next two are re-exported from "Data.GraphViz"
      AttributeNode,
      AttributeEdge,
      dotizeGraph,
      toPosGraph,
      getPositions,
      -- ** Cluster functions
      createLookup,
      setCluster,
      assignCluster,
      -- * List functions
      single,
      longerThan,
      addLengths,
      longest,
      groupElems,
      sortMinMax,
      blockPrint,
      shuffle,
      -- * Statistics functions
      mean,
      statistics,
      statistics',
      -- * Other functions
      fixPoint,
      fixPointGraphs,
      fixPointBy,
      sq,
      fI
    ) where

import Data.Graph.Analysis.Types

import Data.Graph.Inductive.Graph
import Data.GraphViz

import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Set as Set
import qualified Data.IntMap as IMap
import Data.IntMap(IntMap)
import Control.Monad
import Control.Arrow
import System.Random
import System.IO.Unsafe(unsafePerformIO)

-- -----------------------------------------------------------------------------

-- | Extracting data from graphs.

-- | The node number of an 'LNode'.
node :: LNode a -> Node
node = fst

-- | The label of an 'LNode'
label :: LNode a -> a
label = snd

-- | Extract the 'Edge' from the 'LEdge'.
edge           :: LEdge b -> Edge
edge (n1,n2,_) = (n1,n2)

-- | The label of an 'LEdge'
eLabel         :: LEdge b -> b
eLabel (_,_,b) = b

-- | Obtain the labels for a list of 'Node's.
--   It is assumed that each 'Node' is indeed present in the given graph.
addLabels    :: (Graph g) => g a b -> [Node] -> [LNode a]
addLabels gr = map (ap (,) (fromJust . lab gr))

-- | Find all the labelled nodes in the graph that match the given predicate.
filterNodes     :: (Graph g) => (g a b -> LNode a -> Bool) -> g a b -> [LNode a]
filterNodes p g = filter (p g) (labNodes g)

-- | Find all the nodes in the graph that match the given predicate.
filterNodes'     :: (Graph g) => (g a b -> Node -> Bool) -> g a b -> [Node]
filterNodes' p g = filter (p g) (nodes g)

-- -----------------------------------------------------------------------------

-- | Manipulating graphs.

-- | Extract the actual 'LNode's from an 'LPath'.
pathValues          :: LPath a -> [LNode a]
pathValues (LP lns) = lns

{- |
   Make the graph undirected, i.e. for every edge from A to B, there
   exists an edge from B to A.  The provided function
   'Data.Graph.Inductive.Basic.undir' duplicates loops as well, which
   isn't wanted.  It is assumed that no edges are already duplicates
   [i.e. if there exists an edge (n1,n2), then there doesn't exist
   (n2,n1)].  This function also preserves edge labels: if two edges
   exist between two nodes with different edge labels, then both edges
   will be duplicated.
-}
undir :: (Eq b, DynGraph gr) => gr a b -> gr a b
undir = gmap dupEdges
    where
      dupEdges (p,n,l,s) = (ps',n,l,ps)
          where
            ps = nub $ p ++ s
            ps' = snd $ partition isLoop ps
            isLoop (_,n') = n == n'

-- | This is a pseudo-inverse of 'undir': any edges that are both successor
--   and predecessor become successor edges only.
oneWay :: (DynGraph g, Eq b) => g a b -> g a b
oneWay = gmap rmPre
    where
      rmPre (p,n,l,s) = (p \\ s,n,l,s)

-- | Map over the labels on the nodes, using the node values as well.
nlmap   :: (DynGraph gr) => (LNode a -> c) -> gr a b -> gr c b
nlmap f = gmap f'
    where
      f' (p,n,l,s) = (p,n,f (n,l),s)

-- -----------------------------------------------------------------------------

{- |
   Spatial positioning of graphs.  Use the 'graphToGraph' function in
   "Data.GraphViz" to determine potential graph layouts.
-}

-- | Pass the plain graph through 'graphToGraph'.  This is an IO action,
--   however since the state doesn't change it's safe to use 'unsafePerformIO'
--   to convert this to a normal function.
dotizeGraph   :: (DynGraph gr, Ord b) => gr a b
              -> gr (AttributeNode a) (AttributeEdge b)
dotizeGraph g = unsafePerformIO
                $ graphToGraph g gAttrs noAttrs noAttrs
    where
      gAttrs = []
      noAttrs = const []

-- | Convert the graph into one with positions stored in the node labels.
toPosGraph :: (DynGraph gr, Ord b) => gr a b -> gr (PosLabel a) b
toPosGraph = nlmap getPos . emap rmAttrs . dotizeGraph
    where
      rmAttrs = snd
      isPoint attr = case attr of
                       (Pos _) -> True
                       _       -> False
      getPos (n,(as,l)) = PLabel { xPos   = x
                                 , yPos   = y
                                 , pnode  = n
                                 , plabel = l
                                 }
          where
            -- Assume that positions can't be doubles.
            (Pos (PointList ((Point x y):_))) = fromJust $ find isPoint as

-- | Returns the positions of the nodes in the graph, as found using Graphviz.
getPositions :: (DynGraph gr, Ord b) => gr a b -> [PosLabel a]
getPositions = map label . labNodes . toPosGraph

-- -----------------------------------------------------------------------------

-- | Cluster utility functions.

-- | Create a cluster-lookup 'IntMap'.
createLookup :: [[Node]] -> IntMap Int
createLookup = IMap.fromList . concatMap addCluster . zip [1..]
    where
      addCluster (k,ns) = map (flip (,) k) ns

-- | Used when the clusters are assigned in a lookup 'IntMap' instance.
setCluster   :: (DynGraph gr) => IntMap Int -> gr a b -> gr (GenCluster a) b
setCluster m = nlmap assClust
    where
      assClust (n,l) = GC (m IMap.! n) l

-- | A function to convert an 'LNode' to the required 'NodeCluster'
--   for use with the 'Graphviz' library.
assignCluster :: (ClusterLabel a c) => LNode a -> NodeCluster c a
assignCluster nl@(_,a) = C (cluster a) (N nl)

-- -----------------------------------------------------------------------------

-- | List utility functions.

-- | Return true if and only if the list contains a single element.
single     :: [a] -> Bool
single [_] = True
single  _  = False

-- | If we need to only tell if the list contains more than @n@ elements,
--   there's no need to find its length.
longerThan   :: Int -> [a] -> Bool
longerThan n = not . null . drop n

-- | Add the length of each sublist.
addLengths :: [[a]] -> [(Int,[a])]
addLengths = map ( \ as -> (length as, as))

-- | Returns the longest list in a list of lists.
longest :: [[a]] -> [a]
longest = snd . maximumBy (compare `on` fst)
          . addLengths

-- | Group elements by the given grouping function.
groupElems   :: (Ord b) => (a -> b) -> [a] -> [(b,[a])]
groupElems f = map createGroup
               . groupBy ((==) `on` fst)
               . sortBy (compare `on` fst)
               . map addOrd
    where
      addOrd a = (f a, a)
      createGroup bas@((b,_):_) = (b, map snd bas)
      -- This shouldn't ever happen, but let's suppress the -Wall warning.
      createGroup []            = error "Grouping resulted in an empty list!"

-- | Returns the unique elements of the list in ascending order,
--   as well as the minimum and maximum elements.
sortMinMax    :: (Ord a) => [a] -> ([a],a,a)
sortMinMax as = (as',aMin,aMax)
    where
      aSet = Set.fromList as
      as' = Set.toAscList aSet
      aMin = Set.findMin aSet
      aMax = Set.findMax aSet

-- | Attempt to convert a list of elements into a square format
--   in as much of a square shape as possible.
blockPrint    :: (Show a) => [a] -> String
blockPrint as = init -- Remove the final '\n' on the end.
                . unlines $ map unwords lns
    where
      las = addLengths $ map show as
      -- Scale this, to take into account the height:width ratio.
      sidelen :: Double -- Suppress defaulting messages
      sidelen = (1.75*) . sqrt . fromIntegral . sum $ map fst las
      slen = round sidelen
      serr = round $ sidelen/10
      lns = unfoldr (takeLen slen serr) las

-- | Using the given line length and allowed error, take the elements of
--   the next line.
takeLen :: Int -> Int -> [(Int,String)] -> Maybe ([String],[(Int,String)])
takeLen _   _   []          = Nothing
takeLen len err ((l,a):als) = Just lr
    where
      lmax = len + err
      lr = if l > len
           then ([a],als) -- Overflow line of single item
           else (a:as,als')
      -- We subtract one here to take into account the space.
      (as,als') = takeLine (lmax - l - 1) als

-- | Recursively build the rest of the line with given maximum length.
takeLine :: Int -> [(Int,String)] -> ([String],[(Int,String)])
takeLine len als
    | null als  = ([],als)
    | len <= 0  = ([],als) -- This should be covered by the next guard,
                           -- but just in case...
    | l > len   = ([],als)
    | otherwise = (a:as,als'')
    where
      ((l,a):als') = als
      len' = len - l - 1 -- Subtract 1 to account for the space
      (as,als'') = takeLine len' als'

{- |
   Shuffle a list of elements.
   This isn't the most efficient version, but should serve for small lists.
   Adapted from:
   <http://www.cse.unsw.edu.au/~tsewell/shuffle.html>
   The adaptation mainly involved altering the code so that the new
   random seed is also returned.
 -}
shuffle       :: (RandomGen g) => g -> [a] -> ([a],g)
shuffle g []  = ([],g)
shuffle g [x] = ([x],g)
shuffle g xs  = randomMerge g'' ((shYs,yn),(shZs,zn))
    where
        ((ys, yn), (zs, zn)) = splitAndCount xs (([], 0), ([], 0))
        (shYs,g') = shuffle g ys
        (shZs,g'') = shuffle g' zs

splitAndCount :: [a] -> (([a], Int), ([a], Int)) -> (([a], Int), ([a], Int))
splitAndCount [] result = result
splitAndCount (x : xs) ((ys, yn), (zs, zn)) =
    splitAndCount xs ((x : zs, zn + 1), (ys, yn))

{-
  Taken from the original site:

  The idea is to merge two shuffled lists which come with given sizes.
  If the lists X and Y have sizes n and m, we should pick the first element
  of X with probability n / n + m and the first element of Y with probability
  m / n + m. As X and Y are shuffled, picking the first element is random
  among their original elements, and thus this constitutes a random choice
  of first element from the original set.
 -}
randomMerge :: (RandomGen g) => g -> (([a], Int), ([a], Int)) -> ([a],g)
randomMerge g (([],_),(ys,_))       = (ys,g)
randomMerge g ((xs,_),([],_))       = (xs,g)
randomMerge g ((x:xs,xn),(y:ys,yn)) = if n <= xn
                                      then first (x:) xg
                                      else first (y:) yg
    where
      xg = randomMerge g' ((xs, xn - 1), (y : ys, yn))
      yg = randomMerge g' ((x : xs, xn), (ys, yn - 1))
      (n, g') = randomR (1, xn + yn) g

-- -----------------------------------------------------------------------------

-- | Statistics functions.

-- | An efficient mean function by Don Stewart, available from:
--   <http://cgi.cse.unsw.edu.au/~dons/blog/2008/05/16#fast>
mean :: [Double] -> Double
mean = go 0 0
    where
      go :: Double -> Int -> [Double] -> Double
      go s l []     = s / fromIntegral l
      go s l (x:xs) = go (s+x) (l+1) xs

-- | Calculate the mean and standard deviation of a list of elements.
statistics    :: [Double]
              -> (Double,Double) -- ^ (Mean, Standard Deviation)
statistics as = (av,stdDev)
    where
      av = mean as
      stdDev = sqrt . mean $ map (sq . subtract av) as

-- | Calculate the mean and standard deviation of a list of 'Int' values.
statistics'    :: [Int]
               -> (Int,Int) -- ^ (Mean, Standard Deviation)
statistics' as = (av', stdDev')
    where
      (av,stdDev) = statistics $ map fromIntegral as
      av' = round av
      stdDev' = round stdDev

-- -----------------------------------------------------------------------------

-- | Other utility functions.

-- | Find the fixed point of a function with the given initial value.
fixPoint   :: (Eq a) => (a -> a) -> a -> a
fixPoint f = fixPointBy (==) f

-- | Find the fixed point of a function with the given initial value,
--   using the given equality function.
fixPointBy       :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixPointBy eq f x = if (eq x x')
                    then x'
                    else fixPointBy eq f x'
    where
      x' = f x
-- | Find the fixed point of a graph transformation function.
fixPointGraphs   :: (Eq a, Eq b, Graph g) => (g a b -> g a b) -> g a b -> g a b
fixPointGraphs f = fixPointBy equal f

-- | Squaring a number.
sq   :: (Num a) => a -> a
sq x = x * x

-- | Shorthand for 'fromIntegral'
fI :: (Num a) => Int -> a
fI = fromIntegral

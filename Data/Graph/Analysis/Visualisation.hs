{- |
   Module      : Data.Graph.Analysis.Visualisation
   Description : Graphviz wrapper functions
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   Functions to assist in visualising graphs and components of graphs.
 -}
module Data.Graph.Analysis.Visualisation
    ( -- * Graph visualisation.
      -- $graphviz
      graphviz,
      graphvizClusters,
      assignCluster,
      setDir,
      -- * Showing node groupings.
      -- $other
      showPath,
      showPath',
      showCycle,
      showCycle',
      showNodes,
      showNodes',
      -- * Various printing functions.
      blockPrint,
      blockPrint',
      blockPrintList,
      blockPrintList',
      blockPrintWith,
      blockPrintWith',
    ) where

import Data.Graph.Analysis.Types
import Data.Graph.Analysis.Utils
import Data.Graph.Inductive.Graph
import Data.GraphViz

import Data.List(intersperse, unfoldr)

-- -----------------------------------------------------------------------------

{- $graphviz
   Simple wrappers around the Haskell "Data.GraphViz" library to turn
   'GraphData's into basic 'DotGraph's for processing by the GraphViz
   suite of applications.

   'blankParams' may be useful for creating initial definitions of
   'GraphvizParams', especially for 'graphvizClusters'.
-}

-- | Convert the 'GraphData' into 'DotGraph' format.
graphviz :: (Ord cl) => GraphvizParams nl el cl l
            -> GraphData nl el -> DotGraph Node
graphviz = setDir graphToDot

-- | Convert the clustered 'GraphData' into 'DotGraph' format.
--   Cluster the nodes based upon their 'ClusterLabel' clusters.
graphvizClusters    :: (ClusterLabel nl)
                       => GraphvizParams nl el (Cluster nl) (NodeLabel nl)
                       -> GraphData nl el -> DotGraph Node
graphvizClusters ps = graphviz params
  where
    params = ps { clusterBy = assignCluster
                , clusterID = clustID
                }

-- | A function to convert an 'LNode' to the required 'LNodeCluster'
--   for use with the GraphViz library.
assignCluster       :: (ClusterLabel cl) => LNode cl
                       -> LNodeCluster (Cluster cl) (NodeLabel cl)
assignCluster (n,a) = C (cluster a) $ N (n, nodeLabel a)

-- | A cross between 'applyDirAlg' and 'setDirectedness'.
setDir :: (GraphvizParams nl el cl l -> AGr nl el -> a)
          -> GraphvizParams nl el cl l -> GraphData nl el -> a
setDir f params gd = f params' (graph gd)
  where
    params' = params { isDirected = directedData gd }

-- -----------------------------------------------------------------------------

{- $other
   Printing different lists of labels.
 -}

-- | Print a path, with \"->\" between each element.
showPath :: (Show a) => [a] -> String
showPath = showPath' show

-- | Print a path, with \"->\" between each element.
showPath'      :: (a -> String) -> [a] -> String
showPath' _ [] = ""
showPath' f ls = blockPrint' (l:ls'')
    where
      -- Can't use blockPrintWith above, as it only takes a per-row spacer.
      (l:ls') = map f ls
      ls'' = map ("-> "++) ls'

-- | Print a cycle: copies the first node to the end of the list,
--   and then calls 'showPath'.
showCycle :: (Show a) => [a] -> String
showCycle = showCycle' show

-- | Print a cycle: copies the first node to the end of the list,
--   and then calls 'showPath''.
showCycle'            :: (a -> String) -> [a] -> String
showCycle' _ []       = ""
showCycle' f ls@(l:_) = showPath' f (ls ++ [l])

-- | Show a group of nodes, with no implicit ordering.
showNodes :: (Show a) => [a] -> String
showNodes = showNodes' show

-- | Show a group of nodes, with no implicit ordering.
showNodes'      :: (a -> String) -> [a] -> String
showNodes' _ [] = ""
showNodes' f ls = blockPrint' . addCommas
                $ map f ls
    where
      addCommas []     = []
      addCommas [l]    = [l]
      addCommas (l:ls') = (l ++ ", ") : addCommas ls'

-- -----------------------------------------------------------------------------

-- | Attempt to convert the @String@ form of a list into
--   as much of a square shape as possible, using a single
--   space as a separation string.
blockPrint :: (Show a) => [a] -> String
blockPrint = blockPrintWith " "

-- | Attempt to convert a list of @String@s into a single @String@
--   that is roughly a square shape, with a single space as a row
--   separator.
blockPrint' :: [String] -> String
blockPrint' = blockPrintWith' " "

-- | Attempt to convert the @String@ form of a list into
--   as much of a square shape as possible, separating values
--   with commas.
blockPrintList :: (Show a) => [a] -> String
blockPrintList = blockPrintWith ",  "

-- | Attempt to combine a list of @String@s into as much of a
--   square shape as possible, separating values with commas.
blockPrintList' :: [String] -> String
blockPrintList' = blockPrintWith' ",  "

-- | Attempt to convert the @String@ form of a list into
--   as much of a square shape as possible, using the given
--   separation string between elements in the same row.
blockPrintWith     :: (Show a) => String -> [a] -> String
blockPrintWith str = blockPrintWith' str . map show

-- | Attempt to convert the combined form of a list of @String@s
--   into as much of a square shape as possible, using the given
--   separation string between elements in the same row.
blockPrintWith'        :: String -> [String] -> String
blockPrintWith' sep as = init -- Remove the final '\n' on the end.
                         . unlines $ map unwords' lns
    where
      lsep = length sep
      las = addLengths as
      -- Scale this, to take into account the height:width ratio.
      sidelen :: Double -- Suppress defaulting messages
      sidelen = (1.75*) . sqrt . fromIntegral . sum $ map fst las
      slen = round sidelen
      serr = round $ sidelen/10
      lns = unfoldr (takeLen slen serr lsep) las
      unwords' = concat . intersperse sep

-- | Using the given line length and allowed error, take the elements of
--   the next line.
takeLen                          :: Int -> Int -> Int -> [(Int,String)]
                                 -> Maybe ([String],[(Int,String)])
takeLen _   _   _    []          = Nothing
takeLen len err lsep ((l,a):als) = Just lr
    where
      lmax = len + err
      lr = if l > len
           then ([a],als) -- Overflow line of single item
           else (a:as,als')
      -- We subtract lsep here to take into account the spacer.
      (as,als') = takeLine (lmax - l - lsep) lsep als

-- | Recursively build the rest of the line with given maximum length.
takeLine :: Int -> Int -> [(Int,String)] -> ([String],[(Int,String)])
takeLine len lsep als
    | null als  = ([],als)
    | len <= 0  = ([],als) -- This should be covered by the next guard,
                           -- but just in case...
    | l > len   = ([],als)
    | otherwise = (a:as,als'')
    where
      ((l,a):als') = als
      -- We subtract lsep here to take into account the spacer.
      len' = len - l - lsep
      (as,als'') = takeLine len' lsep als'

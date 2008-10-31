{- |
   Module      : Data.Graph.Analysis.Visualisation
   Description : Graphviz wrapper functions
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   Functions to assist in visualising graphs and components of graphs.
   This module uses code licensed under the 3-Clause BSD license from
   the /mohws/ project:
   <http://code.haskell.org/mohws/src/Util.hs>
 -}
module Data.Graph.Analysis.Visualisation
    ( -- * Graph visualisation
      -- $graphviz
      -- ** Producing 'DotGraph's
      graphviz,
      graphvizClusters,
      -- ** Creating images
      GraphvizOutput(..),
      GraphvizCommand(..),
      runGraphviz,
      runGraphvizCommand,
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
import System.Exit
import System.Process
import Data.Array.IO
import Control.Concurrent
import Control.Exception

-- Import this when the parsing stuff is finished
-- import Text.ParserCombinators.PolyLazy

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
      attrs = Label t : as
      nattrs (_,a) = [Label (show a)]
      eattrs _ = []

-- | Turns the graph into 'DotGraph' format with the given title and graph
--   attributes.  Cluster the nodes based upon their 'ClusterLabel' clusters.
--   Nodes and clusters are labelled, edges aren't.
graphvizClusters :: (Graph g, Show c, ClusterLabel a c, Ord b) =>
                    String -> g a b -> [Attribute] -> DotGraph
graphvizClusters t g as = clusterGraphToDot g atts assignCluster cas nas eas
    where
      atts = Label t : as
      cas c = [Label (show c)]
      nas (_,a) = [Label (nodelabel a)]
      eas _ = []

-- | The possible Graphviz outputs, obtained by running /dot -Txxx/.
--   Note that it is not possible to choose between output variants,
--   and that not all of these may be available on your system.
data GraphvizOutput = Canon
                    | Cmap
                    | Cmapx
                    | Cmapx_np
                    | Dia
                    | DotOutput
                    | Eps
                    | Fig
                    | Gd
                    | Gd2
                    | Gif
                    | Gtk
                    | Hpgl
                    | Imap
                    | Imap_np
                    | Ismap
                    | Jpe
                    | Jpeg
                    | Jpg
                    | Mif
                    | Mp
                    | Pcl
                    | Pdf
                    | Pic
                    | Plain
                    | PlainExt
                    | Png
                    | Ps
                    | Ps2
                    | Svg
                    | Svgz
                    | Tk
                    | Vml
                    | Vmlz
                    | Vrml
                    | Vtx
                    | Wbmp
                    | Xdot
                    | Xlib

instance Show GraphvizOutput where
    show Canon     = "canon"
    show Cmap      = "cmap"
    show Cmapx     = "cmapx"
    show Cmapx_np  = "cmapx_np"
    show Dia       = "dia"
    show DotOutput = "dot"
    show Eps       = "eps"
    show Fig       = "fig"
    show Gd        = "gd"
    show Gd2       = "gd2"
    show Gif       = "gif"
    show Gtk       = "gtk"
    show Hpgl      = "hpgl"
    show Imap      = "imap"
    show Imap_np   = "imap_np"
    show Ismap     = "ismap"
    show Jpe       = "jpe"
    show Jpeg      = "jpeg"
    show Jpg       = "jpg"
    show Mif       = "mif"
    show Mp        = "mp"
    show Pcl       = "pcl"
    show Pdf       = "pdf"
    show Pic       = "pic"
    show Plain     = "plain"
    show PlainExt  = "plain-ext"
    show Png       = "png"
    show Ps        = "ps"
    show Ps2       = "ps2"
    show Svg       = "svg"
    show Svgz      = "svgz"
    show Tk        = "tk"
    show Vml       = "vml"
    show Vmlz      = "vmlz"
    show Vrml      = "vrml"
    show Vtx       = "vtx"
    show Wbmp      = "wbmp"
    show Xdot      = "xdot"
    show Xlib      = "xlib"

-- | The available Graphviz commands.
data GraphvizCommand = DotCmd | Neato | TwoPi | Circo | Fdp

instance Show GraphvizCommand where
    show DotCmd = "dot"
    show Neato  = "neato"
    show TwoPi  = "twopi"
    show Circo  = "circo"
    show Fdp    = "fdp"

-- | Run the recommended Graphviz command on this graph, saving the result
--   to the file provided (note: file extensions are /not/ checked).
runGraphviz         :: DotGraph -> GraphvizOutput -> FilePath -> IO Bool
runGraphviz gr t fp = runGraphvizInternal (commandFor gr) gr t fp

-- | Run the chosen Graphviz command on this graph, saving the result
--   to the file provided (note: file extensions are /not/ checked).
runGraphvizCommand             :: GraphvizCommand -> DotGraph -> GraphvizOutput
                               -> FilePath -> IO Bool
runGraphvizCommand cmd gr t fp = runGraphvizInternal (show cmd) gr t fp

-- | This command should /not/ be available outside this module, as
--   it isn't safe: running an arbitrary command will crash the program.
runGraphvizInternal :: String -> DotGraph -> GraphvizOutput -> FilePath
                    -> IO Bool
runGraphvizInternal cmd gr t fp
    = do pipe <- try $ openFile fp WriteMode
         case pipe of
           (Left _)  -> return False
           (Right f) -> do file <- graphvizWithHandle cmd gr t (flip squirt f)
                           hClose f
                           case file of
                             (Just _) -> return True
                             _        -> return False

{- |
   This function is taken from the /mohws/ project, available under a
   3-Clause BSD license.  The actual function is taken from:
   <http://code.haskell.org/mohws/src/Util.hs>
   It provides an efficient way of transferring data from one 'Handle'
   to another.
 -}
squirt :: Handle -> Handle -> IO ()
squirt rd wr = do
  arr <- newArray_ (0, bufsize-1)
  let loop = do
        r <- hGetArray rd arr bufsize
        if (r == 0)
          then return ()
          else if (r < bufsize)
                then hPutArray wr arr r
                else hPutArray wr arr bufsize >> loop
  loop
    where
      -- This was originally separate
      bufsize :: Int
      bufsize = 4 * 1024

{-

-- Parsing to get graph size... do this later.

parseGraphviz    :: DotGraph -> IO (Maybe DotGraph)
parseGraphviz gr = parseGraphvizInternal (commandFor gr) gr

graphvizSizeInternal        :: String -> DotGraph -> Maybe (Int,Int)
graphvizSizeInternal cmd gr =
    where
      parsed = parseGraphvizInternal cmd gr
      getBB = maybe Nothing (find
      bb2Rect (Bb r) = r
      rectToSize (Rect _ (Point x y)) = (x,y)

-- | Internal parsing command.
parseGraphvizInternal        :: String -> DotGraph -> IO (Maybe DotGraph)
parseGraphvizInternal cmd gr = graphvizWithHandle cmd gr DotOutput parse
    where
      parse h = do res <- hGetContents h
                   let gr' = fst $ runParser readDotGraph res
                   return gr'

-}

-- | Internal command to run Graphviz and process the output.
--   This is /not/ to be made available outside this module.
graphvizWithHandle :: (Show a) => String -> DotGraph -> GraphvizOutput -> (Handle -> IO a)
                   -> IO (Maybe a)
graphvizWithHandle cmd gr t f
    = do (inp, outp, errp, proc) <- runInteractiveCommand command
         forkIO $ hPrint inp gr >> hClose inp
         forkIO $ (hGetContents errp >>= hPutStr stderr >> hClose errp)
         a <- f outp
         -- Don't close outp until f finishes.
         a `seq` hClose outp
         exitCode <- waitForProcess proc
         case exitCode of
           ExitSuccess -> return (Just a)
           _           -> return Nothing
    where
      command = cmd ++ " -T" ++ (show t)

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

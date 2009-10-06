{- |
   Module      : Data.Graph.Analysis.Reporting
   Description : Graphalyze Types and Classes
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines the report framework used.
 -}
module Data.Graph.Analysis.Reporting
    ( -- * Document representation
      -- $document
      Document(..),
      DocumentGenerator(..),
      Location(..),
      DocElement(..),
      DocInline(..),
      GraphSize(..),
      DocGraph,
      -- * Helper functions
      -- $utilities
      today,
      tryCreateDirectory,
      createGraph,
      createSize,
      unDotPath
    ) where

import Data.Graph.Inductive(Node)
import Data.GraphViz

import Data.Maybe(isJust, fromJust)
import Data.Time(getZonedTime, zonedTimeToLocalTime, formatTime)
import Control.Exception.Extensible(SomeException(..), tryJust)
import System.Directory(createDirectoryIfMissing)
import System.FilePath((</>), (<.>))
import System.Locale(defaultTimeLocale)

-- -----------------------------------------------------------------------------

{- $document
   'Document' is the simplified representation of a document.  Note
   that this just specifies a document layout, and not an
   implementation.  To actually create a \"physical\" document,
   you must use an instance of 'DocumentGenerator'.
-}

{- | Representation of a document.  The document is to be stored in
   the directory 'rootDirectory', and the main file is to have a
   filename of @'fileFront' '<.>' ('docExtension' dg)@, where @dg@ is an
   instance of 'DocumentGenerator'.
 -}
data Document = Doc { -- | Document location
                      rootDirectory  :: FilePath,
                      fileFront      :: String,
                      -- | The sub-directory of 'rootDirectory',
                      --   where graphs are to be created.
                      graphDirectory :: FilePath,
                      -- | Pre-matter
                      title          :: DocInline,
                      author         :: String,
                      date           :: String,
                      -- | Main-matter
                      content        :: [DocElement]
                    }

-- | Represents the class of document generators.
class DocumentGenerator dg where
    -- | Convert idealised 'Document' values into actual documents,
    --   returning the document file created.
    createDocument :: dg -> Document -> IO (Maybe FilePath)
    -- | The extension of all document-style files created.  Note that
    --   this doesn't preclude the creation of other files, e.g. images.
    docExtension   :: dg -> String

-- | Representation of a location, either on the internet or locally.
data Location = Web String | File FilePath

instance Show Location where
    show (Web url) = url
    show (File fp) = fp

-- | Elements of a document.
data DocElement = Section DocInline [DocElement]
                | Paragraph [DocInline]
                | Enumeration [DocElement]
                | Itemized [DocElement]
                | Definitions [(DocInline, DocInline)]
                | GraphImage DocGraph

-- | Inline elements of a document.
data DocInline = Text String
               | BlankSpace
               | Grouping [DocInline]
               | Bold DocInline
               | Emphasis DocInline
               | DocLink DocInline Location
               | DocImage DocInline Location

-- | Specify the 'DotGraph' to turn into an image, its filename (sans
--   extension) and its caption.  The 'DotGraph' should not have a
--   'Size' set.
type DocGraph = (FilePath, DocInline, DotGraph Node)

-- -----------------------------------------------------------------------------

{- $utilities
   Utility functions to help with document creation.
 -}

-- | Return today's date as a string, e.g. \"Monday 1 January, 2000\".
--   This arbitrary format is chosen as there doesn't seem to be a way
--   of determining the correct format as per the user's locale settings.
today :: IO String
today = do zoneT <- getZonedTime
           let localT = zonedTimeToLocalTime zoneT
           return $ formatTime locale fmt localT
    where
      locale = defaultTimeLocale
      fmt = "%A %e %B, %Y"

-- | Attempts to create the specified directly, returning @True@
--   if successful (or if the directory already exists), @False@
--   if an error occurred.
tryCreateDirectory    :: FilePath -> IO Bool
tryCreateDirectory fp = do r <- tryJust (\(SomeException _) -> return ())
                                $ mkDir fp
                           return (isRight r)
    where
      mkDir = createDirectoryIfMissing True
      isRight (Right _) = True
      isRight _         = False

-- | Attempts to creates a png file (with the given filename in the
--   given directory) from the graph using the given attributes.
--   If the second set of attributes is not 'Nothing', then the first
--   image links to the second.  The whole result is wrapped in a
--   'Paragraph'.
createGraph :: FilePath -> GraphSize -> Maybe GraphSize
            -> DocGraph -> IO (Maybe DocElement)
createGraph gfp s ms (fn,inl,ag)
    = do eImg <- gI s Png "png" DocImage fn inl Nothing
         if isJust eImg
            then case ms of
                   Nothing    -> rt eImg
                   (Just s') -> rt =<< gI s' Svg "svg" DocLink fn' (toImg eImg) eImg
            else return Nothing
    where
      fn' = fn ++ "-large"
      i2e i = Just (i,Paragraph [i])
      rt = return . fmap snd
      -- This is safe because of the isJust above.
      toImg = fst . fromJust
      gI a o e ln nm lb fl = do mImg <- graphImage gfp a o e ln (nm,lb,ag)
                                case mImg of
                                  Nothing    -> return fl
                                  (Just img) -> return $ i2e img

-- | Create the inline image/link from the given DocGraph.
graphImage :: FilePath -> GraphSize
           -> GraphvizOutput -> FilePath
           -> (DocInline -> Location -> DocInline)
           -> DocGraph -> IO (Maybe DocInline)
graphImage gfp s output ext link (fn,inl,dg)
    = do created <- runGraphviz dg' output filename
         if created
            then return (Just img)
            else return Nothing
    where
      dg' = setSize s dg
      fn' = unDotPath fn
      filename = gfp </> fn' <.> ext
      loc = File filename
      img = link inl loc

-- | Specify the size the 'DotGraph' should be at.
data GraphSize = GivenSize Point  -- ^ Specify the size to use.
               | DefaultSize      -- ^ Let GraphViz choose an appropriate size.

-- | Add a 'GlobalAttribute' to the 'DotGraph' specifying the given size.
setSize                 :: GraphSize -> DotGraph a -> DotGraph a
setSize DefaultSize   g = g
setSize (GivenSize p) g = g { graphStatements = stmts' }
    where
      stmts = graphStatements g
      stmts' = stmts { attrStmts = a : (attrStmts stmts) }
      a = GraphAttrs [s]
      s = Size p

-- | Using a 6:4 ratio, create the given 'Point' representing
--   width,height from the width.
createSize   :: Double -> GraphSize
createSize w = GivenSize $ PointD w (w*4/6)

-- | Replace all @.@ with @-@ in the given 'FilePath', since some output
--   formats (e.g. LaTeX) don't like extraneous @.@'s in the filename.
unDotPath :: FilePath -> FilePath
unDotPath = map replace
    where
      replace '.' = '-'
      replace c   = c

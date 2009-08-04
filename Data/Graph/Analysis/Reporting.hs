{- |
   Module      : Data.Graph.Analysis.Reporting
   Description : Graphalyze Types and Classes
   Copyright   : (c) Ivan Lazar Miljenovic 2008
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
      DocGraph,
      -- * Helper functions
      -- $utilities
      today,
      tryCreateDirectory,
      createGraph,
      createSize,
      unDotPath
    ) where

import Data.GraphViz
import Data.Graph.Analysis.Visualisation

import Data.Maybe
import Data.Time
import Control.Exception.Extensible
import System.Directory
import System.FilePath
import System.Locale

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
                | Definition DocInline DocElement
                | GraphImage DocGraph

-- | Inline elements of a document.
data DocInline = Text String
               | BlankSpace
               | Grouping [DocInline]
               | Bold DocInline
               | Emphasis DocInline
               | DocLink DocInline Location
               | DocImage DocInline Location

-- | Let the 'DocumentGenerator' instance apply extra settings, such as size.
type AttrsToGraph = [Attribute] -> DotGraph

type DocGraph = (FilePath,DocInline,AttrsToGraph)

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
createGraph :: FilePath -> FilePath -> [Attribute] -> Maybe [Attribute]
            -> DocGraph -> IO (Maybe DocElement)
createGraph fp gfp as mas (fn,inl,ag)
    = do eImg <- gI as DocImage fn inl Nothing
         if (isJust eImg)
            then case mas of
                   Nothing    -> rt eImg
                   (Just as') -> do rt =<< gI as' DocLink fn' (toImg eImg) eImg
            else return Nothing
    where
      fn' = fn ++ "-large"
      i2e i = Just (i,Paragraph [i])
      rt = return . fmap snd
      toImg = fst . fromJust
      gI a ln nm lb fl = do mImg <- graphImage fp gfp a ln (nm,lb,ag)
                            case mImg of
                              Nothing    -> return fl
                              (Just img) -> return $ i2e img

-- | Create the inline image/link from the given DocGraph.
graphImage :: FilePath -> FilePath -> [Attribute]
           -> (DocInline -> Location -> DocInline)
           -> DocGraph -> IO (Maybe DocInline)
graphImage fp gfp as link (fn,inl,ag)
    = do created <- runGraphviz dg output filename'
         if created
            then return (Just img)
            else return Nothing
    where
      dg = ag as
      fn' = unDotPath fn
      ext = "png"
      output = Png
      filename = gfp </> fn' <.> ext
      filename' = fp </> filename
      loc = File filename
      img = link inl loc

-- | Create the "Data.GraphViz" 'Size' 'Attribute' using the given width
--   and a 6:4 width:height ratio.
createSize   :: Double -> Attribute
createSize w = Size $ PointD w (w*4/6)

-- | Replace all @.@ with @-@ in the given 'FilePath', since some output
--   formats (e.g. LaTeX) don't like extraneous @.@'s in the filename.
unDotPath :: FilePath -> FilePath
unDotPath = map replace
    where
      replace '.' = '-'
      replace c   = c

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
      maximumSize
    ) where

import Data.GraphViz
import Data.GraphViz.Attributes
import Data.Graph.Analysis.Visualisation

import Data.Time
import Control.Exception
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
                      rootDirectory :: FilePath,
                      fileFront     :: String,
                      -- | Pre-matter
                      title         :: DocInline,
                      author        :: String,
                      date          :: String,
                      -- | Main-matter
                      content       :: [DocElement]
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
data Location = URL String | File FilePath

instance Show Location where
    show (URL url) = url
    show (File fp) = fp

-- | Elements of a document.
data DocElement = Section DocInline [DocElement]
                | Paragraph [DocInline]
                | Enumeration [DocElement]
                | Itemized [DocElement]
                | Definition DocInline DocElement
                | DocImage DocInline Location
                | GraphImage DocGraph
                  deriving (Show)

data DocInline = Text String
               | BlankSpace
               | Grouping [DocInline]
               | Bold DocInline
               | Emphasis DocInline
               | DocLink DocInline Location
                 deriving (Show)

type DocGraph = (FilePath,DocInline,DotGraph)

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
tryCreateDirectory fp = do r <- try $ mkDir fp
                           return (isRight r)
    where
      mkDir = createDirectoryIfMissing True
      isRight (Right _) = True
      isRight _         = False

-- | Attempts to creates a png file (with the given filename in the
--   given directory) and - if successful - returns a 'DocImage' link.
createGraph                :: FilePath -> DocGraph -> IO (Maybe DocElement)
createGraph fp (fn,inl,dg) = do created <- runGraphviz dg output filename'
                                if created
                                   then return (Just img)
                                   else return Nothing
    where
      ext = "png"
      output = Png
      filename = fn <.> ext
      filename' = fp </> filename
      loc = File filename
      img = DocImage inl loc

-- | The recommended maximum size for graphs.
maximumSize :: Attribute
maximumSize = Size 15 10

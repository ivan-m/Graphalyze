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
      DocElements(..),
      -- * Helper functions
      -- $utilities
      today,
      tryCreateDirectory
      -- * Result analysis
      -- $analfuncts
      -- hello
    ) where

import Data.Graph.Analysis.Types
import Data.Graph.Analysis.Utils
import Data.Graph.Analysis.Algorithms.Directed(rootsOf)

import Data.List
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
   filename of @'fileFront' ++ 'docExtension' dg@, where @dg@ is an
   instance of 'DocumentGenerator'.
 -}
data Document = Doc { -- | Document location
                      rootDirectory :: FilePath,
                      fileFront     :: String,
                      -- | Pre-matter
                      title         :: String,
                      author        :: String,
                      date          :: String,
                      -- | Main-matter
                      content       :: [DocElements]
                    }

-- | Represents the class of document generators.
class DocumentGenerator dg where
    -- | Convert idealised 'Document' values into actual documents,
    --   returning a list of all files created.
    createDocument :: dg -> Document -> IO ([FilePath])
    -- | The extension of all document-style files created.  Note that
    --   this doesn't preclude the creation of other files, e.g. images.
    docExtension   :: dg -> String

-- | Representation of a location, either on the internet or locally.
data Location = URL String | File FilePath

instance Show Location where
    show (URL url) = url
    show (File fp) = fp

-- | Elements of a document.
data DocElements = Section String [DocElements]
                 | Paragraph String
                 | Text String
                 | Enumeration [DocElements]
                 | Itemized [DocElements]
                 | Link String Location
                 | Image String Location

-- -----------------------------------------------------------------------------

{- $utilities
   Utility functions to help with document creation.
 -}

-- | Return today's date as a string, e.g. "Monday 1 January, 2008".
--   This arbitrary format is chosen as there doesn't seem to be a way
--   of determining the correct format as per the user's locale settings.
today :: IO String
today = do zoneT <- getZonedTime
           let localT = zonedTimeToLocalTime zoneT
               date = formatTime locale fmt localT
           return date
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

-- -----------------------------------------------------------------------------

{- $analfuncts
   Extra functions for data analysis.
 -}

-- | Returns the mean and standard deviations of the lengths of the sublists,
--   as well all those lists more than one standard deviation longer than
--   the mean.
lengthAnalysis    :: [[a]] -> (Int,Int,[(Int,[a])])
lengthAnalysis as = (av,stdDev,as'')
    where
      as' = addLengths as
      ls = map fst as'
      (av,stdDev) = statistics' ls
      as'' = filter (\(l,_) -> l > (av+stdDev)) as'

{- |
   Compare the actual roots in the graph with those that are expected
   (i.e. those in 'wantedRoots').  Returns (in order):

   * Those roots that are expected (i.e. elements of 'wantedRoots'
     that are roots).

   * Those roots that are expected but not present (i.e. elements of
     'wantedRoots' that /aren't/ roots.

   * Unexpected roots (i.e. those roots that aren't present in
     'wantedRoots').
 -}
classifyRoots    :: (Eq a) => GraphData a -> ([LNode a], [LNode a], [LNode a])
classifyRoots gd = (areWanted, notRoots, notWanted)
    where
      g = graph gd
      wntd = wantedRoots gd
      roots = rootsOf g
      areWanted = intersect wntd roots
      notRoots = wntd \\ roots
      notWanted = roots \\ wntd

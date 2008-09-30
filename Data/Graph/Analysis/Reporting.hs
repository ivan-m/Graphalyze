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
      -- hello
    ) where

import Data.Time
import System.Locale
import System.FilePath

-- -----------------------------------------------------------------------------

{- $document
   'Document' is the simplified representation of a document.  Note
   that this just specifies a document layout, and not an
   implementation.  To actually create a \"physical\" document,
   you must use an instance of 'DocumentGenerator'.
-}

{- | Representation of a document.  The document is to be stored in
   the directory 'rootDirectory', and the main file is to have a
   filename of @'fileFront' ++ 'docExtension'@, where the value of
   'docExtension' depends on which instance of 'DocumentGenerator' is
   used.
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
    createDocument :: Document -> IO ([FilePath])
    -- | The extension of all document-style files created.  Note that
    --   this doesn't preclude the creation of other files, e.g. images.
    docExtension   :: String

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

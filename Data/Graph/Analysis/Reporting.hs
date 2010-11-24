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
      DocGraph(..),
      VisParams(..),
      VisProperties(..),
      -- * Helper functions
      -- $utilities
      addLegend,
      today,
      tryCreateDirectory,
      createGraph,
      createSize,
      unDotPath
    ) where

import Data.Graph.Inductive(Node)
import Data.GraphViz

import Data.Time(getZonedTime, zonedTimeToLocalTime, formatTime)
import Control.Exception.Extensible(SomeException(..), tryJust)
import System.Directory(createDirectoryIfMissing)
import System.FilePath(makeRelative)
import System.FilePath.Posix((</>))
import System.Locale(defaultTimeLocale)
import Control.Monad(liftM, when)

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
                      rootDirectory  :: FilePath
                    , fileFront      :: String
                      -- | The sub-directory of 'rootDirectory',
                      --   where graphs are to be created.
                    , graphDirectory :: FilePath
                      -- | Pre-matter
                    , title          :: DocInline
                    , author         :: String
                    , date           :: String
                      -- | Main-matter
                    , legend         :: [(Either DocGraph DocInline, DocInline)]
                    , content        :: [DocElement]
                    }
                deriving (Eq, Ord, Show, Read)

-- | Represents the class of document generators.
class DocumentGenerator dg where
    -- | Convert idealised 'Document' values into actual documents,
    --   returning the document file created.
    createDocument :: dg -> Document -> IO (Maybe FilePath)
    -- | The extension of all document-style files created.  Note that
    --   this doesn't preclude the creation of other files, e.g. images.
    docExtension   :: dg -> String

-- | Representation of a location, either on the internet or locally.
data Location = Web String
              | File FilePath
              deriving (Eq, Ord, Show, Read)

-- | Elements of a document.
data DocElement = Section DocInline [DocElement]
                | Paragraph [DocInline]
                | Enumeration [DocElement]
                | Itemized [DocElement]
                | Definitions [(DocInline, DocInline)]
                | GraphImage DocGraph
                deriving (Eq, Ord, Show, Read)

-- | Inline elements of a document.
data DocInline = Text String
               | BlankSpace
               | Grouping [DocInline]
               | Bold DocInline
               | Emphasis DocInline
               | DocLink DocInline Location
               | DocImage DocInline Location
               deriving (Eq, Ord, Show, Read)

-- | Specify the 'DotGraph' to turn into an image, its filename (sans
--   extension) and its caption.  The 'DotGraph' should not have a
--   'Size' set.
data DocGraph = DG {  -- | What name to provide the image file
                      --   (without an extension).
                     imageFile   :: FilePath
                   , description :: DocInline
                   , dotGraph    :: DotGraph Node
                   }
              deriving (Eq, Ord, Show, Read)

-- | Defines the parameters used for creating visualisations of
--   graphs.
data VisParams = VParams { -- | Root directory of the document.
                           rootDir      :: FilePath
                           -- | Image sub-directory.
                         , graphDir     :: FilePath
                           -- | The default visualisation.
                         , defaultImage :: VisProperties
                           -- | If @'Just' vp'@, then a larger
                           --   visualisation is linked to from the
                           --   default one.
                         , largeImage   :: Maybe VisProperties
                           -- | Should the Dot source code be saved as well?
                         , saveDot      :: Bool
                         }
               deriving (Eq, Ord, Show, Read)

-- | A specification on how to visualise a 'DocGraph'.
data VisProperties = VProps { size   :: GraphSize
                            , format :: GraphvizOutput
                            }
                   deriving (Eq, Ord, Show, Read)

-- | Specify the size the 'DotGraph' should be at.
data GraphSize = GivenSize Point  -- ^ Specify the size to use.
               | DefaultSize      -- ^ Let GraphViz choose an appropriate size.
               deriving (Eq, Ord, Show, Read)

-- -----------------------------------------------------------------------------

{- $utilities
   Utility functions to help with document creation.
 -}

-- | Create the legend section and add it to the document proper.
addLegend             :: FilePath -> FilePath -> VisProperties
                         -> Document -> IO Document
addLegend fp gfp vp d = do mLg <- legendToElement fp gfp vp $ legend d
                           let es = content d
                               es' = maybe es (flip (:) es) mLg
                           return $ d { legend  = []
                                      , content = es'
                                      }

legendToElement           :: FilePath -> FilePath -> VisProperties
                             -> [(Either DocGraph DocInline, DocInline)]
                             -> IO (Maybe DocElement)
legendToElement _  _   _  [] = return Nothing
legendToElement fp gfp vp ls = do defs <- mapM (uncurry (legToDef fp gfp vp)) ls
                                  let df   = Definitions defs
                                  return $ Just $ Section (Text "Legend") [df]

legToDef :: FilePath -> FilePath -> VisProperties
            -> Either DocGraph DocInline -> DocInline
            -> IO (DocInline, DocInline)
legToDef fp gfp vp (Left dg) def = liftM ((,) def)
                                   $ graphImage' fp gfp vp' dg
  where
    vp' = vp { size = DefaultSize }
legToDef _ _ _ (Right di) def = return (def,di)

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

-- | Attempts to create image files (with the given filename in the
--   given directory) from the graph.  If the second 'VisProperties'
--   not 'Nothing', then the first image links to the second.  The
--   whole result is wrapped in a 'Paragraph'.  'unDotPath' is applied
--   to the filename in the 'DocGraph'.
createGraph :: VisParams -- ^ Visualisation parameters.
               -> DocGraph
               -> IO DocElement
createGraph params dg
  = do when (saveDot params) (graphImage rDir gDir vpD dgD >> return ())
       dl  <- graphImage' rDir gDir vp dg'
       dl' <- maybe return tryImg mvp dl
       return $ Paragraph [dl']
  where
    rDir = rootDir params
    gDir = graphDir params
    vp = defaultImage params
    vpD = VProps { size   = DefaultSize
                 , format = Canon
                 }
    mvp = largeImage params
    dg' = dg { imageFile = unDotPath $ imageFile dg }
    dgL = checkLargeFilename vp mvp dg'
    dgD = checkFilename vp vpD "dot" dg'
    tryImg vp' di = liftM (either (const di) (DocLink di))
                   $ graphImage rDir gDir vp' dgL

-- | If both output formats are the same, then the larger image needs
--   a different filename.
checkLargeFilename :: VisProperties -> Maybe VisProperties
                      -> DocGraph -> DocGraph
checkLargeFilename _   Nothing    dg = dg
checkLargeFilename vp1 (Just vp2) dg = checkFilename vp1 vp2 "large" dg

checkFilename :: VisProperties -> VisProperties -> String
                 -> DocGraph -> DocGraph
checkFilename vp1 vp2 s dg
  | format vp1 == format vp2 = dg { imageFile = imageFile dg ++ '-' : s }
  | otherwise                = dg

graphImage :: FilePath -> FilePath -> VisProperties -> DocGraph
              -> IO (Either DocInline Location)
graphImage rDir gDir vp dg = liftM (either' Text (File . fixPath))
                             $ addExtension (runGraphviz dot)
                                            (format vp)
                                            filename
  where
    dot = setSize vp $ dotGraph dg
    filename = rDir </> gDir </> imageFile dg
    fixPath = makeRelative rDir

graphImage' :: FilePath -> FilePath -> VisProperties -> DocGraph
               -> IO DocInline
graphImage' rDir gDir vp dg = liftM (either id f)
                              $ graphImage rDir gDir vp dg
  where
    f = DocImage (description dg)

-- | Add a 'GlobalAttribute' to the 'DotGraph' specifying the given size.
setSize      :: VisProperties -> DotGraph a -> DotGraph a
setSize vp g = case size vp of
                 DefaultSize   -> g
                 (GivenSize s) -> g { graphStatements = setS s}
  where
    setS s = stmts { attrStmts = sizeA s : attrStmts stmts }
    stmts = graphStatements g
    sizeA s = GraphAttrs [Size s]

-- | Using a 6:4 ratio, create the given 'Point' representing
--   width,height from the width.
createSize   :: Double -> GraphSize
createSize w = GivenSize $ createPoint w (w*4/6)

-- | Replace all @.@ with @-@ in the given 'FilePath', since some output
--   formats (e.g. LaTeX) don't like extraneous @.@'s in the filename.
unDotPath :: FilePath -> FilePath
unDotPath = map replace
    where
      replace '.' = '-'
      replace c   = c

-- | Map either element of an 'Either'.
either'                 :: (a -> c) -> (b -> d) -> Either a b -> Either c d
either' fl _  (Left a)  = Left  $ fl a
either' _  fr (Right b) = Right $ fr b

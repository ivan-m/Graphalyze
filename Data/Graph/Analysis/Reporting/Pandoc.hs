{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Data.Graph.Analysis.Reporting.Pandoc
   Description : Graphalyze Types and Classes
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module uses /Pandoc/ to generate documents:
   <http://johnmacfarlane.net/pandoc/>

   Note that Pandoc is released under GPL-2 or later, however
   according to the Free Software Foundation, I am indeed allowed to
   use it:
   <http://www.fsf.org/licensing/licenses/gpl-faq.html#IfLibraryIsGPL>
   since the 2-Clause BSD license that I'm using is GPL-compatible:
   <http://www.fsf.org/licensing/licenses/index_html#GPLCompatibleLicenses>
   (search for /FreeBSD License/, which is another name for it).
 -}
module Data.Graph.Analysis.Reporting.Pandoc
    ( PandocDocument,
      pandocHtml,
      pandocLaTeX,
      pandocRtf,
      pandocMarkdown,
      alsoSaveDot
    ) where

-- TODO : the ability to create multiple files.

import Data.Graph.Analysis.Reporting

import           Data.GraphViz.Commands (GraphvizOutput(Png, Svg))
import           Text.Pandoc
import qualified Text.Pandoc.Shared     as P

import Control.Arrow     ((***))
import Control.Exception (SomeException, try)
import Data.List         (intersperse)
import Data.Maybe        (fromJust, isNothing)
import System.Directory  (removeDirectoryRecursive)
import System.FilePath   ((<.>), (</>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- -----------------------------------------------------------------------------

{- $writers
   The actual exported writers.
 -}

pandocHtml :: PandocDocument
pandocHtml = pd { writer        = writeHtml5String
                , extension     = "html"
                , templateName  = "html"
                , extGraphProps = Just VProps { grSize = DefaultSize
                                              , format = Svg
                                              }
                }

pandocLaTeX :: PandocDocument
pandocLaTeX = pd { writer     = writeLaTeX
                 , extension  = "tex"
                 , templateName = "latex"
                 -- 4.5" should be less than \textwidth in LaTeX.
                 , graphProps = defaultProps { grSize = createSize 4.5 }
                 }

pandocRtf :: PandocDocument
pandocRtf = pd { writer    = writeRTF
               , extension = "rtf"
               , templateName = "rtf"
               }

pandocMarkdown :: PandocDocument
pandocMarkdown = pd { writer = writeMarkdown
                    , extension = "text"
                    , templateName = "markdown"
                    }

-- -----------------------------------------------------------------------------

{- $defs
   Pandoc definition.
 -}

-- | Definition of a Pandoc Document.  Size measurements are in inches,
--   and a 6:4 ratio is used for width:length.
data PandocDocument = PD { -- | The Pandoc document style
                           writer        :: WriterOptions -> Pandoc -> PandocPure Text
                           -- | The file extension used
                         , extension     :: FilePath
                           -- | Which template to get.
                         , templateName  :: Text
                           -- | Size of graphs to be produced.
                         , graphProps    :: VisProperties
                           -- | Optional size of external linked graphs.
                         , extGraphProps :: Maybe VisProperties
                           -- | Should the Dot source code be saved as well?
                         , keepDot       :: Bool
                         }

-- | Some default sizes.  Note that all other fields of 'PandocDocument'
--   still need to be defined.
pd :: PandocDocument
pd = PD { writer        = undefined
        , extension     = undefined
        , templateName  = undefined
        , graphProps    = defaultProps
        , extGraphProps = Nothing
        , keepDot       = False
        }

-- | Also save the generated Dot code to file when creating visualisations.
alsoSaveDot   :: PandocDocument -> PandocDocument
alsoSaveDot p = p { keepDot = True }

defaultWidth :: Double
defaultWidth = 10

defaultProps :: VisProperties
defaultProps = VProps { grSize = createSize defaultWidth
                      , format = Png
                      }

instance DocumentGenerator PandocDocument where
    createDocument = createPandoc
    docExtension   = extension

-- | Define the 'WriterOptions' used.
writerOptions :: WriterOptions
writerOptions = def { writerTableOfContents = True
                    , writerNumberSections = True
                    }

-- | Used when traversing the document structure.
data PandocProcess = PP { secLevel  :: Int
                        , visParams :: VisParams
                        }
                   deriving (Eq, Ord, Show, Read)

-- | Start with a level 1 heading.
defaultProcess :: PandocProcess
defaultProcess = PP { secLevel  = 1
                    , visParams = undefined
                    }

-- | Create the document.
createPandoc     :: PandocDocument -> Document -> IO (Maybe FilePath)
createPandoc p d = do
  created <- tryCreateDirectory dir
  -- If the first one fails, so will this one.
  _ <- tryCreateDirectory $ dir </> gdir
  if not created
    then failDoc
    else do
      d' <- addLegend dir gdir (graphProps p) d
      elems <- multiElems pp $ content d'
      case elems of
        Just es -> do
          let es' = htmlAuthDt : es
              pnd = Pandoc meta es'
          case runPure $ convert pnd of
            Left _ -> failDoc
            Right text -> do
              doc <- tryWrite text
              case doc of
                (Right _) -> success
                (Left _)  -> failDoc
        Nothing -> failDoc
 where
      dir = rootDirectory d
      gdir = graphDirectory d
      auth = (Text.pack . author) d
      dt = (Text.pack . date) d
      meta = makeMeta (title d) auth dt
      -- Html output doesn't show date and auth anywhere by default.
      htmlAuthDt = htmlInfo auth dt
      pp = defaultProcess { visParams = vp }
      vp = VParams { rootDir      = dir
                   , graphDir     = gdir
                   , defaultImage = graphProps p
                   , largeImage   = extGraphProps p
                   , saveDot      = keepDot p
                   }
      convert = writer p writerOptions
      file = dir </> fileFront d <.> extension p
      tryWrite :: Text -> IO (Either SomeException ())
      tryWrite = try . Text.writeFile file
      success = return (Just file)
      failDoc = removeDirectoryRecursive dir >> return Nothing

-- -----------------------------------------------------------------------------

{- $conv
   Converting individual elements to their corresponding Pandoc types.
 -}

-- | The meta information
makeMeta         :: DocInline -> Text -> Text -> Meta
makeMeta tle a t = P.makeMeta (inlines tle) [[Str a]] [Str t]

-- | Html output doesn't show the author and date; use this to print it.
htmlInfo         :: Text -> Text -> Block
htmlInfo auth dt = RawBlock (Format "html") html
    where
      heading = "<h1>Document Information</h1>"
      html = Text.unlines [heading, htmlize auth, htmlize dt]
      htmlize str = "<blockquote><p><em>" <> str <> "</em></p></blockquote>"

-- | Link conversion
loc2target             :: Location -> Target
loc2target (Web url)   = (Text.pack url,"")
loc2target (File file) = (Text.pack file,"")

-- | Conversion of simple inline elements.
inlines                    :: DocInline -> [Inline]
inlines (Text str)         = [Str (Text.pack str)]
inlines BlankSpace         = [Space]
inlines (Grouping grp)     = concat . intersperse [Space] $ map inlines grp
inlines (Bold inl)         = [Strong (inlines inl)]
inlines (Emphasis inl)     = [Emph (inlines inl)]
inlines (DocLink inl loc)  = [Link nullAttr (inlines inl) (loc2target loc)]
inlines (DocImage inl loc) = [Image nullAttr (inlines inl) (loc2target loc)]

{- |
   Conversion of complex elements.  The only reason it's in the IO monad is
   for GraphImage, as it requires IO to create the image.

   If any one of the conversions fail (i.e. returns 'Nothing'), the entire
   process fails.

   In future, may extend this to creating multiple files for top-level
   sections, in which case the IO monad will be required for that as
   well.
-}
elements :: PandocProcess -> DocElement -> IO (Maybe [Block])

elements p (Section lbl elems) = do let n = secLevel p
                                        p' = p { secLevel = n + 1}
                                        sec = Header n nullAttr (inlines lbl)
                                    elems' <- multiElems p' elems
                                    return (fmap (sec:) elems')

elements _ (Paragraph inls)    = return $ Just [Para (concatMap inlines inls)]

elements p (Enumeration elems) = do elems' <- multiElems' p elems
                                    let attrs = (1,DefaultStyle,DefaultDelim)
                                        list = fmap (OrderedList attrs) elems'
                                    return (fmap return list)

elements p (Itemized elems)    = do elems' <- multiElems' p elems
                                    return (fmap (return . BulletList) elems')

elements _ (Definitions defs)  = return . Just . return . DefinitionList
                                 $ map (inlines *** ((:[]) . (:[])
                                                     . Plain . inlines))
                                       defs

elements p (GraphImage dg)     = elements p =<< createGraph (visParams p) dg

-- | Concatenate the result of multiple calls to 'elements'.
multiElems         :: PandocProcess -> [DocElement] -> IO (Maybe [Block])
multiElems p elems = do elems' <- mapM (elements p) elems
                        if any isNothing elems'
                           then return Nothing
                           else return (Just $ concatMap fromJust elems')

-- | As for 'multiElems', but don't @concat@ the resulting 'Block's.
multiElems'         :: PandocProcess -> [DocElement] -> IO (Maybe [[Block]])
multiElems' p elems = do elems' <- mapM (elements p) elems
                         if any isNothing elems'
                            then return Nothing
                            else return (Just $ map fromJust elems')

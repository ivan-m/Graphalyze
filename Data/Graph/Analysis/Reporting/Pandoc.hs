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
      pandocMarkdown
    ) where

-- TODO : the ability to create multiple files.

import Data.Graph.Analysis.Reporting
import Data.GraphViz.Attributes(Attribute)

import Data.List
import Data.Maybe
import Text.Pandoc
import Control.Monad
import Control.Exception
import System.Directory
import System.FilePath

-- -----------------------------------------------------------------------------

{- $writers
   The actual exported writers.
 -}

pandocHtml :: PandocDocument
pandocHtml = pd { writer       = writeHtmlString
                , extension    = "html"
                , header       = "" -- Header will be included
                , extGraphSize = Just $ defaultWidth * 10
                }

pandocLaTeX :: PandocDocument
pandocLaTeX = pd { writer    = writeLaTeX
                 , extension = "tex"
                 , header    = defaultLaTeXHeader
                 -- 4.5" should be less than \textwidth in LaTeX.
                 , graphSize = 4.5
                 }

pandocRtf :: PandocDocument
pandocRtf = pd { writer    = writeRTF
               , extension = "rtf"
               , header    = defaultRTFHeader
               }

pandocMarkdown :: PandocDocument
pandocMarkdown = pd { writer = writeMarkdown
                    , extension = "text"
                    , header = ""
                    }

-- -----------------------------------------------------------------------------

{- $defs
   Pandoc definition.
 -}

-- | Definition of a Pandoc Document.  Size measurements are in inches,
--   and a 6:4 ratio is used for width:length.
data PandocDocument = PD { -- | The Pandoc document style
                           writer       :: WriterOptions -> Pandoc -> String,
                           -- | The file extension used
                           extension    :: FilePath,
                           -- | The Pandoc header to use
                           header       :: String,
                           -- | Maximum width of graphs to be produced.
                           graphSize    :: Double,
                           -- | Optional maximum width of external linked graph.
                           extGraphSize :: Maybe Double
                         }

-- | Some default sizes.  Note that all other fields of 'PandocDocument'
--   still need to be defined.
pd :: PandocDocument
pd = PD { writer       = undefined,
          extension    = undefined,
          header       = undefined,
          graphSize    = defaultWidth,
          extGraphSize = Nothing
        }

defaultWidth :: Double
defaultWidth = 10

instance DocumentGenerator PandocDocument where
    createDocument = createPandoc
    docExtension   = extension

-- | Define the 'WriterOptions' used.
writerOptions :: WriterOptions
writerOptions = defaultWriterOptions { writerStandalone = True
                                     , writerTableOfContents = True
                                     , writerNumberSections = True
                                     }

-- | Used when traversing the document structure.
data PandocProcess = PP { secLevel :: Int
                        , filedir  :: FilePath
                        , graphdir :: FilePath
                        , grSize   :: [Attribute]
                        , eGSize   :: Maybe [Attribute]
                        }

-- | Start with a level 1 heading.
defaultProcess :: PandocProcess
defaultProcess = PP { secLevel = 1
                    , graphdir = undefined
                    , filedir  = undefined
                    , grSize   = undefined
                    , eGSize   = undefined
                    }

-- | Create the document.
createPandoc     :: PandocDocument -> Document -> IO (Maybe FilePath)
createPandoc p d = do created <- tryCreateDirectory dir
                      -- If the first one fails, so will this one.
                      tryCreateDirectory $ dir </> gdir
                      if not created
                         then failDoc
                         else do elems <- multiElems pp (content d)
                                 case elems of
                                   Just es -> do let es' = htmlAuthDt : es
                                                     pnd = Pandoc meta es'
                                                     doc = convert pnd
                                                 wr <- tryWrite doc
                                                 case wr of
                                                   (Right _) -> success
                                                   (Left _)  -> failDoc
                                   Nothing -> failDoc
    where
      dir = rootDirectory d
      gdir = graphDirectory d
      auth = author d
      dt = date d
      meta = makeMeta (title d) auth dt
      -- Html output doesn't show date and auth anywhere by default.
      htmlAuthDt = htmlInfo auth dt
      createSize' = return . createSize
      pp = defaultProcess { filedir = dir
                          , graphdir = gdir
                          , grSize = createSize' (graphSize p)
                          , eGSize = fmap createSize' (extGraphSize p)
                          }
      opts = writerOptions { writerHeader = (header p) }
      convert = writer p opts
      file = dir </> fileFront d <.> extension p
      tryWrite :: String -> IO (Either SomeException ())
      tryWrite = try . writeFile file
      success = return (Just file)
      failDoc = removeDirectoryRecursive dir >> return Nothing

-- -----------------------------------------------------------------------------

{- $conv
   Converting individual elements to their corresponding Pandoc types.
 -}

-- | The meta information
makeMeta     :: DocInline -> String -> String -> Meta
makeMeta t a = Meta (inlines t) [a]

-- | Html output doesn't show the author and date; use this to print it.
htmlInfo         :: String -> String -> Block
htmlInfo auth dt = RawHtml html
    where
      heading = "<h1>Document Information</h1>"
      html = unlines [heading, htmlize auth, htmlize dt]
      htmlize str = "<blockquote><p><em>" ++ str ++ "</em></p></blockquote>"

-- | Link conversion
loc2target             :: Location -> Target
loc2target (Web url)   = (url,"")
loc2target (File file) = (file,"")

-- | Conversion of simple inline elements.
inlines                    :: DocInline -> [Inline]
inlines (Text str)         = [Str str]
inlines BlankSpace         = [Space]
inlines (Grouping grp)     = concat . intersperse [Space] $ map inlines grp
inlines (Bold inl)         = [Strong (inlines inl)]
inlines (Emphasis inl)     = [Emph (inlines inl)]
inlines (DocLink inl loc)  = [Link (inlines inl) (loc2target loc)]
inlines (DocImage inl loc) = [Image (inlines inl) (loc2target loc)]

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
                                        sec = Header n (inlines lbl)
                                    elems' <- multiElems p' elems
                                    return (fmap (sec:) elems')

elements _ (Paragraph inls)    = return $ Just [Para (concatMap inlines inls)]

elements p (Enumeration elems) = do elems' <- multiElems' p elems
                                    let attrs = (1,DefaultStyle,DefaultDelim)
                                        list = fmap (OrderedList attrs) elems'
                                    return (fmap return list)

elements p (Itemized elems)    = do elems' <- multiElems' p elems
                                    return (fmap (return . BulletList) elems')

elements p (Definition x def)  = do def' <- elements p def
                                    let x' = inlines x
                                        xdef = fmap (return . (,) x') def'
                                    return (fmap (return . DefinitionList) xdef)

elements p (GraphImage dg)     = do el <- createGraph (filedir p)
                                                      (graphdir p)
                                                      (grSize p)
                                                      (eGSize p) dg
                                    case el of
                                      Nothing  -> return Nothing
                                      Just img -> elements p img

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

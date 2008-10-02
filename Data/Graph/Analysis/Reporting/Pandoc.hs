{- |
   Module      : Data.Graph.Analysis.Reporting.Pandoc
   Description : Graphalyze Types and Classes
   Copyright   : (c) Ivan Lazar Miljenovic 2008
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
    (

    ) where

import Data.Graph.Analysis.Reporting

import Data.List
import Data.Maybe
import Text.Pandoc.Definition
import Control.Monad

loc2target             :: Location -> Target
loc2target (URL url)   = (url,"")
loc2target (File file) = (file,"")

-- data PandocReport = PD {

data PandocProcess = PP { secLevel :: Int
                        , filedir  :: FilePath
                        }

pandocExt :: FilePath
pandocExt = "md"

-- -----------------------------------------------------------------------------

{- $conv
   Converting individual elements to their corresponding Pandoc types.
 -}

-- | Conversion of simple inline elements.
inlines                   :: DocInline -> [Inline]
inlines (Text str)        = intersperse Space $ map Str (words str)
inlines (Grouping grp)    = concat . intersperse [Space] $ map inlines grp
inlines (Bold inl)        = [Strong (inlines inl)]
inlines (Emphasis inl)    = [Emph (inlines inl)]
inlines (DocLink inl loc) = [Link (inlines inl) (loc2target loc)]

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

elements _ (DocImage inl loc)  = do let img = Image (inlines inl)
                                                    (loc2target loc)
                                    return $ Just [Plain [img]]

elements p (GraphImage dg)     = do elem <- createGraph (filedir p) dg
                                    case elem of
                                      Nothing  -> return Nothing
                                      Just img -> elements p img

-- | Concatenate the result of multiple calls to 'elements'.
multiElems         :: PandocProcess -> [DocElement] -> IO (Maybe [Block])
multiElems p elems = do elems' <- mapM (elements p) elems
                        if (any isNothing elems')
                           then return Nothing
                           else return (Just $ concatMap fromJust elems')


-- | As for 'multiElems', but don't @concat@ the resulting 'Block's.
multiElems'         :: PandocProcess -> [DocElement] -> IO (Maybe [[Block]])
multiElems' p elems = do elems' <- mapM (elements p) elems
                         if (any isNothing elems')
                            then return Nothing
                            else return (Just $ map fromJust elems')

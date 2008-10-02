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
import Text.Pandoc.Definition

loc2target             :: Location -> Target
loc2target (URL url)   = (url,"")
loc2target (File file) = (file,"")

inlines                    :: DocInline -> [Inline]
inlines (Text str)         = intersperse Space $ map Str (words str)
inlines (Grouping grp)     = concat . intersperse [Space] $ map inlines grp
inlines (Bold inl)         = [Strong (inlines inl)]
inlines (Emphasis inl)     = [Emph (inlines inl)]
inlines (DocLink inl loc)  = [Link (inlines inl) (loc2target loc)]
inlines (DocImage inl loc) = [Image (inlines inl) (loc2target loc)]

elements                       :: Int -> DocElement -> [Block]
elements n (Section lbl elems) = Header n (inlines lbl)
                                 : concatMap (elements (n+1)) elems
elements _ (Paragraph inls)    = [Para (concatMap inlines inls)]
elements n (Enumeration elems) = let attrs = (1,DefaultStyle,DefaultDelim) in
                                 [OrderedList attrs (map (elements n) elems)]
elements n (Itemized elems)    = [BulletList (map (elements n) elems)]
elements n (Definition x def)  = [DefinitionList [(inlines x, elements n def)]]

{-
 - Copyright (c) 2014 Jorge Israel Peña <jorge.israel.p@gmail.com>
 - Copyright (c) 2017 Alexandre Lucchesi <alexandrelucchesi@gmail.com>
 -
 - All rights reserved.
 -
 - Copyright (c) 2014 Jorge Israel Peña <jorge.israel.p@gmail.com>, All rights
 - reserved.
 -
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 -
 - 1. Redistributions of source code must retain the above copyright notice,
 -    this list of conditions and the following disclaimer.
 - 2. Redistributions in binary form must reproduce the above copyright notice,
 -    this list of conditions and the following disclaimer in the documentation
 -    and/or other materials provided with the distribution.
 - 3. Neither the name of the copyright holder nor the names of its contributors
 -    may be used to endorse or promote products derived from this software
 -    without specific prior written permission.
 -
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 - AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 - IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 - DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 - FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 - DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 - SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 - CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 - OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 - OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Site.TableOfContents (
  tableOfContents,
  ignoreTOC,
  collectHeaders,
  removeTOCMarker
) where

import Text.Pandoc
import Text.Pandoc.Walk (walk, query)

import Data.List (groupBy)
import Data.Tree (Forest, Tree(Node))
import Data.Monoid ((<>), mconcat)
import Data.Function (on)
import Data.Maybe (fromMaybe)

import Text.Blaze.Html (preEscapedToHtml, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

headerLevel :: Block -> Int
headerLevel (Header level _ _) = level
headerLevel _ = error "not a header"

ignoreTOC :: Block -> Block
ignoreTOC (Header level (ident, classes, params) inline) =
  Header level (ident, "notoc" : classes, params) inline
ignoreTOC x = x

removeTOCMarker :: Block -> Block
removeTOCMarker (BulletList (( (( Plain ((Str "toc"):_)):_)):_)) = Null
removeTOCMarker x = x

collectHeaders :: Block -> [Block]
collectHeaders header@(Header _ (_, classes, _) _) =
  if "notoc" `elem` classes
    then []
    else [header]
collectHeaders _ = []

groupByHierarchy :: [Block] -> Forest Block
groupByHierarchy = map (\(x:xs) -> Node x (groupByHierarchy xs)) . groupBy ((<) `on` headerLevel)

markupHeader :: Tree Block -> H.Html
markupHeader (Node (Header _ (ident, _, keyvals) inline) headers)
  | headers == [] = H.li $ link
  | otherwise     = H.li $ link <> (H.ol $ markupHeaders headers)
  where render x  = writeHtmlString def (Pandoc nullMeta [(Plain x)])
        section   = fromMaybe (render inline) (lookup "toc" keyvals)
        link      = H.a ! A.href (H.toValue $ "#" ++ ident) $ preEscapedToHtml section
markupHeader _ = error "what"

markupHeaders :: Forest Block -> H.Html
markupHeaders = mconcat . map markupHeader

createTable :: Forest Block -> H.Html
createTable headers =
  (H.nav ! A.id "toc") $ do
    --H.h3 "Contents"
    --H.ol $ markupHeaders headers
    H.ul $ markupHeaders headers

generateTOC :: [Block] -> String -> Block -> Block
generateTOC [] _     x = x
generateTOC headers alignment x@(BulletList (( (( Plain ((Str "toc"):_)):_)):_))
  | alignment == "right" = render . (! A.class_ "right-toc") . table $ headers
  | alignment == "left"  = render . table $ headers
  | otherwise            = x
  where render = (RawBlock "html") . renderHtml
        table  = createTable . groupByHierarchy
generateTOC _ _ x = x

tableOfContents :: String -> Pandoc -> Pandoc
tableOfContents alignment ast =
  if alignment /= "off"
    then let headers = query collectHeaders ast
         in walk (generateTOC headers alignment) ast
    else walk ignoreTOC ast


-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Text
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  David Terei <code@davidterei.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Provides a collection of pretty printer combinators, a set of API's
-- that provides a way to easily print out text in a consistent format
-- of your choosing.
--
-- This module should be used as opposed to the 'Text.PrettyPrint.HughesPJ'
-- module. Both are equivalent though as this module simply re-exports the
-- other.
--
-----------------------------------------------------------------------------

module Text (

        -- * The document type
        Doc,

        -- * Constructing documents

        -- ** Converting values into documents
        PP.char, text, ptext, sizedText, zeroWidthText,
        PP.int, PP.integer, PP.float, PP.double, PP.rational,

        -- ** Simple derived documents
        PP.semi, PP.comma, PP.colon, PP.space, PP.equals,
        PP.lparen, PP.rparen, PP.lbrack, PP.rbrack, PP.lbrace, PP.rbrace,

        -- ** Wrapping documents in delimiters
        PP.parens, PP.brackets, PP.braces, PP.quotes, PP.doubleQuotes,

        -- ** Combining documents
        PP.empty,
        (PP.<>), (PP.<+>), PP.hcat, PP.hsep,
        (PP.$$), (PP.$+$), PP.vcat,
        PP.sep, PP.cat,
        PP.fsep, PP.fcat,
        PP.nest,
        PP.hang, PP.punctuate,

        -- * Predicates on documents
        PP.isEmpty,

        -- * Rendering documents

        -- ** Default rendering
        render,

        -- ** Rendering with a particular style
        PP.Style(..),
        PP.style,
        renderStyle,

        -- ** General rendering
        PP.fullRender,
        PP.Mode(..), TextDetails

    ) where

import qualified Text.PrettyPrint.Annotated.HughesPJ as PP
import Data.Text as Text
import Data.Text.IO as Text

type Doc a = PP.Doc Text a

text :: Text -> Doc a
text = PP.text
ptext :: Text -> Doc a
ptext = PP.ptext
sizedText :: Int -> Text -> Doc a
sizedText = PP.sizedText
zeroWidthText :: Text -> Doc a
zeroWidthText = PP.zeroWidthText
render :: Doc a -> Text
render = PP.render
renderStyle :: PP.Style -> Doc a -> Text
renderStyle = PP.renderStyle

type TextDetails = PP.TextDetails Text

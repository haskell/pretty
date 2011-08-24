{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint2
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David Terei <dave.terei@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A set of Pretty Printing Combinators, presenting the core of the
-- pretty Pretty Printing library.
-- .
-- Here a set of combinators for handling the String and Char types
-- are presented but the library can handle other underlying String
-- representations. For this usage, see @Text.PrettyPrint.Core@ where
-- the core API of @PrettyPrint@ resides but with the string type
-- abstracted out with through the @DocBase@ type class.
--
-----------------------------------------------------------------------------

module Text.PrettyPrint2 (
        -- * The document type
        Doc,

        -- * Constructing documents

        -- ** Converting values into documents
        char, text, ptext, sizedText, zeroWidthText,
        int, integer, float, double, rational,

        -- ** Simple derived documents
        semi, comma, colon, space, equals,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace,

        -- ** Wrapping documents in delimiters
        parens, brackets, braces, quotes, doubleQuotes,

        -- ** Combining documents
        empty,
        (<>), (<+>), hcat, hsep,
        ($$), ($+$), vcat,
        sep, cat,
        fsep, fcat,
        nest,
        hang, punctuate,

        -- * Predicates on documents
        isEmpty,

        -- * Utility functions for documents
        first,

        -- * Rendering documents

        -- ** Default rendering
        render, renderInMode,

        -- ** Rendering with a particular style
        Style(..),
        style,
        renderStyle,

        -- ** General rendering
        fullRender,
        Mode(..), TextDetails(..)
    ) where

import Text.PrettyPrint.Core

-- | The TextDetails data type
--
-- A TextDetails represents a fragment of text that will be
-- output at some point.
data TextDetails = Chr  {-# UNPACK #-} !Char
                               -- ^ A single Char fragment
                 | Str  String -- ^ A whole String fragment
                 | PStr String -- ^ Used to represent a Fast String fragment
                               --   but now deprecated and identical to the
                               --   Str constructor.

instance DocBase TextDetails where
    chr = Chr
    str = Str

type Doc = GDoc TextDetails

-- Displaying @Doc@ values.
{-
instance Show (GDoc TextDetails) where
  showsPrec _ doc cont = showDocWithAppend PageMode doc cont
-}

-- | Redundant, use text instead
ptext :: String -> Doc
ptext = text

render :: Doc -> String
render doc = showDoc doc ""

renderInMode:: Mode -> Doc -> String
renderInMode m doc = showDocWithAppend m doc ""

showDocWithAppend :: Mode -> Doc -> String -> String
showDocWithAppend m doc rest
  = fullRender m (lineLength style) (ribbonsPerLine style) string_txt rest doc

renderStyle :: Style -> Doc -> String
renderStyle the_style doc
  = fullRender (mode the_style)
               (lineLength the_style)
               (ribbonsPerLine the_style)
               string_txt
               ""
               doc

showDoc :: Doc -> String -> String
showDoc doc rest = fullRender PageMode 100 1.5 string_txt rest doc

string_txt :: TextDetails -> String -> String
string_txt (Chr c)   s  = c:s
string_txt (Str s1)  s2 = s1 ++ s2
string_txt (PStr s1) s2 = s1 ++ s2


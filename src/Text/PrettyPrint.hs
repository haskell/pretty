{-# LANGUAGE FlexibleInstances #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David Terei <dave.terei@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A set of Pretty Printing Combinators, presenting the core of the
-- pretty Pretty Printing library.
--
-- Here a set of combinators for handling the String and Char types
-- are presented but the library can handle other underlying String
-- representations. For this usage, see @Text.PrettyPrint.Core@ where
-- the core API of @PrettyPrint@ resides but with the string type
-- abstracted out with through the @DocBase@ type class.
--
-----------------------------------------------------------------------------

module Text.PrettyPrint (
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
        render,

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

instance DocText TextDetails where
    chr = Chr
    str = Str

-- | The abstract type of documents.
-- The 'Show' instance is equivalent to using 'render'.
type Doc = GDoc TextDetails

-- | Displaying @Doc@ values.
instance Show (GDoc TextDetails) where
  showsPrec _ doc cont = showDocWithAppend PageMode doc cont

-- | Redundant, use text instead
ptext :: String -> Doc
ptext = text
{-# DEPRECATED ptext "Please use text instead" #-}

-- | Renders the document as a string using the default 'style'.
render :: Doc -> String
render doc = showDocWithAppend PageMode doc ""

showDocWithAppend :: Mode -> Doc -> String -> String
showDocWithAppend m doc rest
  = fullRender m (lineLength style) (ribbonsPerLine style) txtPrinter rest doc

-- | Render the document as a string using a specified style.
renderStyle :: Style -> Doc -> String
renderStyle the_style doc
  = fullRender (mode the_style)
               (lineLength the_style)
               (ribbonsPerLine the_style)
               txtPrinter
               ""
               doc

txtPrinter :: TextDetails -> String -> String
txtPrinter (Chr c)   s  = c:s
txtPrinter (Str s1)  s2 = s1 ++ s2
txtPrinter (PStr s1) s2 = s1 ++ s2


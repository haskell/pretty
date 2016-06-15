#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.TextLazy
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

module Text.PrettyPrint.Annotated.TextLazy (

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
import Data.Text.Lazy (Text)

type Doc = PP.Doc Text

{-
char :: Char -> Doc a
char = PP.char
-}
text :: Text -> Doc a
text = PP.text
ptext :: Text -> Doc a
ptext = PP.ptext
sizedText :: Int -> Text -> Doc a
sizedText = PP.sizedText
zeroWidthText :: Text -> Doc a
zeroWidthText = PP.zeroWidthText
{-
int :: Int -> Doc a
int = PP.int
integer :: Integer -> Doc a
integer = PP.integer
float :: Float -> Doc a
float = PP.float
double :: Double -> Doc a
double = PP.double
rational :: Rational -> Doc a
rational = PP.rational
semi :: Doc a
semi = PP.semi
comma :: Doc a
comma = PP.comma
colon :: Doc a
colon = PP.colon
space :: Doc a
space = PP.space
equals :: Doc a
equals = PP.equals
lparen :: Doc a
lparen = PP.lparen
rparen :: Doc a
rparen = PP.rparen
lbrack :: Doc a
lbrack = PP.lbrack
rbrack :: Doc a
rbrack = PP.rbrack
lbrace :: Doc a
lbrace = PP.lbrace
rbrace :: Doc a
rbrace = PP.rbrace
parens :: Doc a -> Doc a
parens = PP.parens
brackets :: Doc a -> Doc a
brackets = PP.brackets
braces :: Doc a -> Doc a
braces = PP.braces
quotes :: Doc a -> Doc a
quotes = PP.quotes
doubleQuotes :: Doc a -> Doc a
doubleQuotes = PP.doubleQuotes
empty :: Doc a
empty = PP.empty
(<>) :: Doc a -> Doc a -> Doc a
(<>) = (PP.<>)
(<+>) :: Doc a -> Doc a -> Doc a
(<+>) = (PP.<+>)
hcat :: [Doc a] -> Doc a
hcat = PP.hcat
hsep :: [Doc a] -> Doc a
hsep = PP.hsep
($$) :: Doc a -> Doc a -> Doc a
($$) = (PP.$$)
($+$) :: Doc a -> Doc a -> Doc a
($+$) = (PP.$+$)
vcat :: [Doc a] -> Doc a
vcat = PP.vcat
sep :: [Doc a] -> Doc a
sep = PP.sep
cat :: [Doc a] -> Doc a
cat = PP.cat
fsep :: [Doc a] -> Doc a
fsep = PP.fsep
fcat :: [Doc a] -> Doc a
fcat = PP.fcat
nest :: Int -> Doc a -> Doc a
nest = PP.nest
hang :: Doc a -> Int -> Doc a -> Doc a
hang = PP.hang
punctuate :: Doc a -> [Doc a] -> [Doc a]
punctuate = PP.punctuate
isEmpty :: Doc a -> Bool
isEmpty = PP.isEmpty
-}
render :: Doc a -> Text
render = PP.render
renderStyle :: PP.Style -> Doc a -> Text
renderStyle = PP.renderStyle
{-
fullRender :: PP.Mode -> Int -> Float -> (TextDetails -> a -> a) -> a -> Doc b -> a
fullRender = PP.fullRender
-}
type TextDetails = PP.TextDetails Text

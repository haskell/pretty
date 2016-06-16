#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.String
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

module Text.PrettyPrint.String (

        -- * The document type
        PP.Chars, Doc,

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

import qualified Text.PrettyPrint.ListLike as PP

type Doc = PP.Doc String

{-
char :: Char -> Doc a
char = PP.char
-}
text :: String -> Doc
text = PP.text
ptext :: String -> Doc
ptext = PP.ptext
sizedText :: Int -> String -> Doc
sizedText = PP.sizedText
zeroWidthText :: String -> Doc
zeroWidthText = PP.zeroWidthText
{-
int :: Int -> Doc
int = PP.int
integer :: Integer -> Doc
integer = PP.integer
float :: Float -> Doc
float = PP.float
double :: Double -> Doc
double = PP.double
rational :: Rational -> Doc
rational = PP.rational
semi :: Doc
semi = PP.semi
comma :: Doc
comma = PP.comma
colon :: Doc
colon = PP.colon
space :: Doc
space = PP.space
equals :: Doc
equals = PP.equals
lparen :: Doc
lparen = PP.lparen
rparen :: Doc
rparen = PP.rparen
lbrack :: Doc
lbrack = PP.lbrack
rbrack :: Doc
rbrack = PP.rbrack
lbrace :: Doc
lbrace = PP.lbrace
rbrace :: Doc
rbrace = PP.rbrace
parens :: Doc -> Doc
parens = PP.parens
brackets :: Doc -> Doc
brackets = PP.brackets
braces :: Doc -> Doc
braces = PP.braces
quotes :: Doc -> Doc
quotes = PP.quotes
doubleQuotes :: Doc -> Doc
doubleQuotes = PP.doubleQuotes
empty :: Doc
empty = PP.empty
(<>) :: Doc -> Doc -> Doc
(<>) = (PP.<>)
(<+>) :: Doc -> Doc -> Doc
(<+>) = (PP.<+>)
hcat :: [Doc] -> Doc
hcat = PP.hcat
hsep :: [Doc] -> Doc
hsep = PP.hsep
($$) :: Doc -> Doc -> Doc
($$) = (PP.$$)
($+$) :: Doc -> Doc -> Doc
($+$) = (PP.$+$)
vcat :: [Doc] -> Doc
vcat = PP.vcat
sep :: [Doc] -> Doc
sep = PP.sep
cat :: [Doc] -> Doc
cat = PP.cat
fsep :: [Doc] -> Doc
fsep = PP.fsep
fcat :: [Doc] -> Doc
fcat = PP.fcat
nest :: Int -> Doc -> Doc
nest = PP.nest
hang :: Doc -> Int -> Doc -> Doc
hang = PP.hang
punctuate :: Doc -> [Doc] -> [Doc]
punctuate = PP.punctuate
isEmpty :: Doc -> Bool
isEmpty = PP.isEmpty
-}
render :: Doc -> String
render = PP.render
renderStyle :: PP.Style -> Doc -> String
renderStyle = PP.renderStyle
{-
fullRender :: PP.Mode -> Int -> Float -> (TextDetails -> a -> a) -> a -> Doc b -> a
fullRender = PP.fullRender
-}
type TextDetails = PP.TextDetails String

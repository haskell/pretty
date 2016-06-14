{-# OPTIONS_HADDOCK not-home #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.HughesPJ
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David Terei <code@davidterei.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Provides a collection of pretty printer combinators, a set of API's that
-- provides a way to easily print out text in a consistent format of your
-- choosing.
--
-- Originally designed by John Hughes's and Simon Peyton Jones's.
--
-- For more information you can refer to the
-- <http://belle.sourceforge.net/doc/hughes95design.pdf original paper> that
-- serves as the basis for this libraries design: /The Design -- of a
-- Pretty-printing Library/ by John Hughes, in Advanced Functional Programming,
-- 1995.
--
-----------------------------------------------------------------------------

#ifndef TESTING
module Text.PrettyPrint.HughesPJ (

        -- * The document type
        Chars(..), Doc, TextDetails(..),

        -- * Constructing documents

        -- ** Converting values into documents
        char, text, ptext, sizedText, zeroWidthText,
        int, integer, float, double, rational,

        -- ** Simple derived documents
        semi, comma, colon, space, equals,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace,

        -- ** Wrapping documents in delimiters
        parens, brackets, braces, quotes, doubleQuotes,
        maybeParens, maybeBrackets, maybeBraces, maybeQuotes, maybeDoubleQuotes,

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
        first, reduceDoc,

        -- * Rendering documents

        -- ** Default rendering
        render,

        -- ** Rendering with a particular style
        Style(..),
        style,
        renderStyle,
        Mode(..),

        -- ** General rendering
        fullRender

    ) where
#endif

import           Text.PrettyPrint.Annotated.HughesPJ
                     ( Chars(..), TextDetails(..), Mode(..), Style(..), style )
import qualified Text.PrettyPrint.Annotated.HughesPJ as Ann

import Control.DeepSeq ( NFData(rnf) )
import Data.Function   ( on )
#if __GLASGOW_HASKELL__ >= 800
import qualified Data.Semigroup as Semi ( Semigroup((<>)) )
#elif __GLASGOW_HASKELL__ < 709
import Data.Monoid     ( Monoid(mempty, mappend)  )
#endif
import Data.String     ( IsString(fromString) )

import GHC.Generics


-- ---------------------------------------------------------------------------
-- Operator fixity

infixl 6 <>
infixl 6 <+>
infixl 5 $$, $+$

-- ---------------------------------------------------------------------------
-- The Doc data type

-- | The abstract type of documents. A Doc represents a /set/ of layouts. A
-- Doc with no occurrences of Union or NoDoc represents just one layout.
newtype Doc string = Doc (Ann.Doc string ())
#if __GLASGOW_HASKELL__ >= 701
                    deriving (Generic)
#endif

liftList :: ([Ann.Doc string ()] -> Ann.Doc string ()) -> ([Doc string] -> Doc string)
liftList f ds = Doc (f [ d | Doc d <- ds ])
{-# INLINE liftList #-}

liftBinary :: (Ann.Doc string () -> Ann.Doc string () -> Ann.Doc string ())
           -> (    Doc string    ->     Doc string   ->     Doc string   )
liftBinary f (Doc a) (Doc b) = Doc (f a b)
{-# INLINE liftBinary #-}

-- | RDoc is a "reduced GDoc", guaranteed not to have a top-level Above or
-- Beside.
type RDoc = Doc

-- Combining @Doc@ values
#if __GLASGOW_HASKELL__ >= 800
instance Chars string => Semi.Semigroup (Doc string) where
    (<>) = (Text.PrettyPrint.HughesPJ.<>)

instance Chars string => Monoid (Doc string) where
    mempty  = empty
    mappend = (Semi.<>)
#else
instance Chars string => Monoid (Doc string) where
    mempty  = empty
    mappend = (<>)
#endif

instance Chars string => IsString (Doc string) where
    fromString = text . fromString

instance Chars string => Show (Doc string) where
  showsPrec _ doc cont =
             toString $ fullRender (mode style) (lineLength style)
                                    (ribbonsPerLine style)
                                    txtPrinter (fromString cont) doc

instance (Chars string, Eq string) => Eq (Doc string) where
  (==) = (==) `on` render

instance Chars string => NFData (Doc string) where
  rnf (Doc a) = rnf a

-- ---------------------------------------------------------------------------
-- Values and Predicates on GDocs and TextDetails

-- | A document of height and width 1, containing a literal character.
char :: Chars string => Char -> Doc string
char c = Doc (Ann.char c)
{-# INLINE char #-}

-- | A document of height 1 containing a literal string.
-- 'text' satisfies the following laws:
--
-- * @'text' s '<>' 'text' t = 'text' (s'++'t)@
--
-- * @'text' \"\" '<>' x = x@, if @x@ non-empty
--
-- The side condition on the last law is necessary because @'text' \"\"@
-- has height 1, while 'empty' has no height.
text :: Chars string => string -> Doc string
text s = Doc (Ann.text s)
{-# INLINE text #-}

-- | Same as @text@. Used to be used for Bytestrings.
ptext :: Chars string => string -> Doc string
ptext s = Doc (Ann.ptext s)
{-# INLINE ptext #-}

-- | Some text with any width. (@text s = sizedText (length s) s@)
sizedText :: Chars string => Int -> string -> Doc string
sizedText l s = Doc (Ann.sizedText l s)

-- | Some text, but without any width. Use for non-printing text
-- such as a HTML or Latex tags
zeroWidthText :: Chars string => string -> Doc string
zeroWidthText = sizedText 0

-- | The empty document, with no height and no width.
-- 'empty' is the identity for '<>', '<+>', '$$' and '$+$', and anywhere
-- in the argument list for 'sep', 'hcat', 'hsep', 'vcat', 'fcat' etc.
empty :: Doc string
empty = Doc Ann.empty

-- | Returns 'True' if the document is empty
isEmpty :: Doc string -> Bool
isEmpty (Doc d) = Ann.isEmpty d

semi   :: Chars string => Doc string -- ^ A ';' character
comma  :: Chars string => Doc string -- ^ A ',' character
colon  :: Chars string => Doc string -- ^ A ':' character
space  :: Chars string => Doc string -- ^ A space character
equals :: Chars string => Doc string -- ^ A '=' character
lparen :: Chars string => Doc string -- ^ A '(' character
rparen :: Chars string => Doc string -- ^ A ')' character
lbrack :: Chars string => Doc string -- ^ A '[' character
rbrack :: Chars string => Doc string -- ^ A ']' character
lbrace :: Chars string => Doc string -- ^ A '{' character
rbrace :: Chars string => Doc string -- ^ A '}' character
semi   = char ';'
comma  = char ','
colon  = char ':'
space  = char ' '
equals = char '='
lparen = char '('
rparen = char ')'
lbrack = char '['
rbrack = char ']'
lbrace = char '{'
rbrace = char '}'

int      :: Chars string => Int      -> Doc string -- ^ @int n = text (show n)@
integer  :: Chars string => Integer  -> Doc string -- ^ @integer n = text (show n)@
float    :: Chars string => Float    -> Doc string -- ^ @float n = text (show n)@
double   :: Chars string => Double   -> Doc string -- ^ @double n = text (show n)@
rational :: Chars string => Rational -> Doc string -- ^ @rational n = text (show n)@
int      n = text (fromString (show n))
integer  n = text (fromString (show n))
float    n = text (fromString (show n))
double   n = text (fromString (show n))
rational n = text (fromString (show n))

parens       :: Chars string => Doc string -> Doc string -- ^ Wrap document in @(...)@
brackets     :: Chars string => Doc string -> Doc string -- ^ Wrap document in @[...]@
braces       :: Chars string => Doc string -> Doc string -- ^ Wrap document in @{...}@
quotes       :: Chars string => Doc string -> Doc string -- ^ Wrap document in @\'...\'@
doubleQuotes :: Chars string => Doc string -> Doc string -- ^ Wrap document in @\"...\"@
quotes p       = char '\'' <> p <> char '\''
doubleQuotes p = char '"' <> p <> char '"'
parens p       = char '(' <> p <> char ')'
brackets p     = char '[' <> p <> char ']'
braces p       = char '{' <> p <> char '}'

-- | Apply 'parens' to 'Doc' if boolean is true.
maybeParens :: Chars string => Bool -> Doc string -> Doc string
maybeParens False = id
maybeParens True = parens

-- | Apply 'brackets' to 'Doc' if boolean is true.
maybeBrackets :: Chars string => Bool -> Doc string -> Doc string
maybeBrackets False = id
maybeBrackets True = brackets

-- | Apply 'braces' to 'Doc' if boolean is true.
maybeBraces :: Chars string => Bool -> Doc string -> Doc string
maybeBraces False = id
maybeBraces True = braces

-- | Apply 'quotes' to 'Doc' if boolean is true.
maybeQuotes :: Chars string => Bool -> Doc string -> Doc string
maybeQuotes False = id
maybeQuotes True = quotes

-- | Apply 'doubleQuotes' to 'Doc' if boolean is true.
maybeDoubleQuotes :: Chars string => Bool -> Doc string -> Doc string
maybeDoubleQuotes False = id
maybeDoubleQuotes True = doubleQuotes

-- ---------------------------------------------------------------------------
-- Structural operations on GDocs

-- | Perform some simplification of a built up @GDoc@.
reduceDoc :: Chars string => Doc string -> RDoc string
reduceDoc (Doc d) = Doc (Ann.reduceDoc d)
{-# INLINE reduceDoc #-}

-- | List version of '<>'.
hcat :: Chars string => [Doc string] -> Doc string
hcat = liftList Ann.hcat
{-# INLINE hcat #-}

-- | List version of '<+>'.
hsep :: Chars string => [Doc string] -> Doc string
hsep = liftList Ann.hsep
{-# INLINE hsep #-}

-- | List version of '$$'.
vcat :: Chars string => [Doc string] -> Doc string
vcat = liftList Ann.vcat
{-# INLINE vcat #-}

-- | Nest (or indent) a document by a given number of positions
-- (which may also be negative).  'nest' satisfies the laws:
--
-- * @'nest' 0 x = x@
--
-- * @'nest' k ('nest' k' x) = 'nest' (k+k') x@
--
-- * @'nest' k (x '<>' y) = 'nest' k x '<>' 'nest' k y@
--
-- * @'nest' k (x '$$' y) = 'nest' k x '$$' 'nest' k y@
--
-- * @'nest' k 'empty' = 'empty'@
--
-- * @x '<>' 'nest' k y = x '<>' y@, if @x@ non-empty
--
-- The side condition on the last law is needed because
-- 'empty' is a left identity for '<>'.
nest :: Chars string => Int -> Doc string -> Doc string
nest k (Doc p) = Doc (Ann.nest k p)
{-# INLINE nest #-}

-- | @hang d1 n d2 = sep [d1, nest n d2]@
hang :: Chars string => Doc string -> Int -> Doc string -> Doc string
hang (Doc d1) n (Doc d2) = Doc (Ann.hang d1 n d2)
{-# INLINE hang #-}

-- | @punctuate p [d1, ... dn] = [d1 \<> p, d2 \<> p, ... dn-1 \<> p, dn]@
punctuate :: Chars string => Doc string -> [Doc string] -> [Doc string]
punctuate (Doc p) ds = [ Doc d | d <- Ann.punctuate p [ d | Doc d <- ds ] ]
{-# INLINE punctuate #-}


-- ---------------------------------------------------------------------------
-- Vertical composition @$$@

-- | Above, except that if the last line of the first argument stops
-- at least one position before the first line of the second begins,
-- these two lines are overlapped.  For example:
--
-- >    text "hi" $$ nest 5 (text "there")
--
-- lays out as
--
-- >    hi   there
--
-- rather than
--
-- >    hi
-- >         there
--
-- '$$' is associative, with identity 'empty', and also satisfies
--
-- * @(x '$$' y) '<>' z = x '$$' (y '<>' z)@, if @y@ non-empty.
--
($$) :: Chars string => Doc string -> Doc string -> Doc string
($$) = liftBinary (Ann.$$)
{-# INLINE ($$) #-}

-- | Above, with no overlapping.
-- '$+$' is associative, with identity 'empty'.
($+$) :: Chars string => Doc string -> Doc string -> Doc string
($+$) = liftBinary (Ann.$+$)
{-# INLINE ($+$) #-}


-- ---------------------------------------------------------------------------
-- Horizontal composition @<>@

-- We intentionally avoid Data.Monoid.(<>) here due to interactions of
-- Data.Monoid.(<>) and (<+>).  See
-- http://www.haskell.org/pipermail/libraries/2011-November/017066.html

-- | Beside.
-- '<>' is associative, with identity 'empty'.
(<>) :: Chars string => Doc string -> Doc string -> Doc string
(<>) = liftBinary (Ann.<>)
{-# INLINE (<>) #-}

-- | Beside, separated by space, unless one of the arguments is 'empty'.
-- '<+>' is associative, with identity 'empty'.
(<+>) :: Chars string => Doc string -> Doc string -> Doc string
(<+>) = liftBinary (Ann.<+>)
{-# INLINE (<+>) #-}


-- ---------------------------------------------------------------------------
-- Separate, @sep@

-- Specification: sep ps  = oneLiner (hsep ps)
--                         `union`
--                          vcat ps

-- | Either 'hsep' or 'vcat'.
sep  :: Chars string => [Doc string] -> Doc string
sep  = liftList Ann.sep
{-# INLINE sep #-}

-- | Either 'hcat' or 'vcat'.
cat :: Chars string => [Doc string] -> Doc string
cat = liftList Ann.cat
{-# INLINE cat #-}


-- ---------------------------------------------------------------------------
-- @fill@

-- | \"Paragraph fill\" version of 'cat'.
fcat :: Chars string => [Doc string] -> Doc string
fcat = liftList Ann.fcat
{-# INLINE fcat #-}

-- | \"Paragraph fill\" version of 'sep'.
fsep :: Chars string => [Doc string] -> Doc string
fsep = liftList Ann.fsep
{-# INLINE fsep #-}


-- ---------------------------------------------------------------------------
-- Selecting the best layout

-- | @first@ returns its first argument if it is non-empty, otherwise its second.
first :: Chars string => Doc string -> Doc string -> Doc string
first  = liftBinary Ann.first
{-# INLINE first #-}


-- ---------------------------------------------------------------------------
-- Rendering

-- | Render the @Doc@ to a String using the default @Style@ (see 'style').
render :: Chars string => Doc string -> string
render = fullRender (mode style) (lineLength style) (ribbonsPerLine style)
                    txtPrinter mempty
{-# INLINE render #-}

-- | Render the @Doc@ to a String using the given @Style@.
renderStyle :: Chars string => Style -> Doc string -> string
renderStyle s = fullRender (mode s) (lineLength s) (ribbonsPerLine s)
                txtPrinter mempty
{-# INLINE renderStyle #-}

-- | Default TextDetails printer.
txtPrinter :: Chars string => TextDetails string -> string -> string
txtPrinter (Chr c)   s  = cons c s
txtPrinter (Str s1)  s2 = s1 `mappend` s2
txtPrinter (PStr s1) s2 = s1 `mappend` s2

-- | The general rendering interface. Please refer to the @Style@ and @Mode@
-- types for a description of rendering mode, line length and ribbons.
fullRender :: Chars string
           => Mode                     -- ^ Rendering mode.
           -> Int                      -- ^ Line length.
           -> Float                    -- ^ Ribbons per line.
           -> (TextDetails string -> a -> a)  -- ^ What to do with text.
           -> a                        -- ^ What to do at the end.
           -> Doc string                      -- ^ The document.
           -> a                        -- ^ Result.
fullRender m lineLen ribbons txt rest (Doc doc)
  = Ann.fullRender m lineLen ribbons txt rest doc
{-# INLINE fullRender #-}


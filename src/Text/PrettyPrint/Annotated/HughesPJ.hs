{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE BangPatterns #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Annotated.HughesPJ
-- Copyright   :  (c) Trevor Elliott <revor@galois.com> 2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David Terei <code@davidterei.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module provides a version of pretty that allows for annotations to be
-- attached to documents. Annotations are arbitrary pieces of metadata that can
-- be attached to sub-documents.
--
-----------------------------------------------------------------------------

#ifndef TESTING
module Text.PrettyPrint.Annotated.HughesPJ (

        -- * The document type
        Chars, Doc, TextDetails(..), AnnotDetails(..),

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

        -- ** Annotating documents
        annotate,

        -- * Predicates on documents
        isEmpty,

        -- * Utility functions for documents
        first, reduceDoc,

        -- * Rendering documents

        -- ** Default rendering
        render,

        -- ** Annotation rendering
        renderSpans, Span(..),
        renderDecorated,
        renderDecoratedM,

        -- ** Rendering with a particular style
        Style(..),
        style,
        renderStyle,
        Mode(..),

        -- ** General rendering
        fullRender,
        fullRenderAnn

    ) where
#endif

import Control.DeepSeq ( NFData(rnf) )
import Data.Function   ( on )
#if __GLASGOW_HASKELL__ >= 800
import qualified Data.Semigroup as Semi ( Semigroup((<>)) )
#elif __GLASGOW_HASKELL__ < 709
import Data.Monoid     ( Monoid(mempty, mappend)  )
#endif
import Data.String     ( IsString(fromString) )
import Data.ListLike   (ListLike, ListLikeIO, toString, length, cons, fromList, StringLike)
import Data.Text as Strict (Text)
import Data.Text.Lazy as Lazy (Text)

import GHC.Generics
import Prelude hiding (length)

--- | A class which includes only the operations on strings required by
--- pretty.  This class is a subset of ListLike, which is not used
--- because it conflicts with the Safe extension.  No other instanaces
--- are defined here for the same reason.
class (ListLike string Char, ListLikeIO string Char, IsString string, StringLike string, NFData string, Eq string) => Chars string

instance Chars String
instance Chars Strict.Text
instance Chars Lazy.Text

-- ---------------------------------------------------------------------------
-- The Doc calculus

{-
Laws for $$
~~~~~~~~~~~
<a1>    (x $$ y) $$ z   = x $$ (y $$ z)
<a2>    empty $$ x      = x
<a3>    x $$ empty      = x

        ...ditto $+$...

Laws for <>
~~~~~~~~~~~
<b1>    (x <> y) <> z   = x <> (y <> z)
<b2>    empty <> x      = empty
<b3>    x <> empty      = x

        ...ditto <+>...

Laws for text
~~~~~~~~~~~~~
<t1>    text s <> text t        = text (s++t)
<t2>    text "" <> x            = x, if x non-empty

** because of law n6, t2 only holds if x doesn't
** start with `nest'.


Laws for nest
~~~~~~~~~~~~~
<n1>    nest 0 x                = x
<n2>    nest k (nest k' x)      = nest (k+k') x
<n3>    nest k (x <> y)         = nest k x <> nest k y
<n4>    nest k (x $$ y)         = nest k x $$ nest k y
<n5>    nest k empty            = empty
<n6>    x <> nest k y           = x <> y, if x non-empty

** Note the side condition on <n6>!  It is this that
** makes it OK for empty to be a left unit for <>.

Miscellaneous
~~~~~~~~~~~~~
<m1>    (text s <> x) $$ y = text s <> ((text "" <> x) $$
                                         nest (-length s) y)

<m2>    (x $$ y) <> z = x $$ (y <> z)
        if y non-empty


Laws for list versions
~~~~~~~~~~~~~~~~~~~~~~
<l1>    sep (ps++[empty]++qs)   = sep (ps ++ qs)
        ...ditto hsep, hcat, vcat, fill...

<l2>    nest k (sep ps) = sep (map (nest k) ps)
        ...ditto hsep, hcat, vcat, fill...

Laws for oneLiner
~~~~~~~~~~~~~~~~~
<o1>    oneLiner (nest k p) = nest k (oneLiner p)
<o2>    oneLiner (x <> y)   = oneLiner x <> oneLiner y

You might think that the following verion of <m1> would
be neater:

<3 NO>  (text s <> x) $$ y = text s <> ((empty <> x)) $$
                                         nest (-length s) y)

But it doesn't work, for if x=empty, we would have

        text s $$ y = text s <> (empty $$ nest (-length s) y)
                    = text s <> nest (-length s) y
-}

-- ---------------------------------------------------------------------------
-- Operator fixity

infixl 6 <>
infixl 6 <+>
infixl 5 $$, $+$

-- ---------------------------------------------------------------------------
-- The Doc data type

-- | The abstract type of documents. A Doc represents a /set/ of layouts. A Doc
-- with no occurrences of Union or NoDoc represents just one layout.
data Doc string a
  = Empty                                              -- ^ An empty span, see 'empty'.
  | NilAbove (Doc string a)                            -- ^ @text "" $$ x@.
  | TextBeside !(AnnotDetails string a) (Doc string a) -- ^ @text s <> x@.
  | Nest {-# UNPACK #-} !Int (Doc string a)            -- ^ @nest k x@.
  | Union (Doc string a) (Doc string a)                -- ^ @ul `union` ur@.
  | NoDoc                                              -- ^ The empty set of documents.
  | Beside (Doc string a) Bool (Doc string a)          -- ^ True <=> space between.
  | Above (Doc string a) Bool (Doc string a)           -- ^ True <=> never overlap.
#if __GLASGOW_HASKELL__ >= 701
  deriving (Generic)
#endif

{-
Here are the invariants:

1) The argument of NilAbove is never Empty. Therefore a NilAbove occupies at
least two lines.

2) The argument of @TextBeside@ is never @Nest@.

3) The layouts of the two arguments of @Union@ both flatten to the same string.

4) The arguments of @Union@ are either @TextBeside@, or @NilAbove@.

5) A @NoDoc@ may only appear on the first line of the left argument of an
   union. Therefore, the right argument of an union can never be equivalent to
   the empty set (@NoDoc@).

6) An empty document is always represented by @Empty@. It can't be hidden
   inside a @Nest@, or a @Union@ of two @Empty@s.

7) The first line of every layout in the left argument of @Union@ is longer
   than the first line of any layout in the right argument. (1) ensures that
   the left argument has a first line. In view of (3), this invariant means
   that the right argument must have at least two lines.

Notice the difference between
   * NoDoc (no documents)
   * Empty (one empty document; no height and no width)
   * text "" (a document containing the empty string; one line high, but has no
              width)
-}


-- | RDoc is a "reduced GDoc", guaranteed not to have a top-level Above or Beside.
type RDoc = Doc

-- | An annotation (side-metadata) attached at a particular point in a @Doc@.
-- Allows carrying non-pretty-printed data around in a @Doc@ that is attached
-- at particular points in the structure. Once the @Doc@ is render to an output
-- type (such as 'String'), we can also retrieve where in the rendered document
-- our annotations start and end (see 'Span' and 'renderSpans').
data AnnotDetails string a
    = AnnotStart
    | NoAnnot !(TextDetails string) {-# UNPACK #-} !Int
    | AnnotEnd a
      deriving (Show,Eq)

instance Functor (AnnotDetails string) where
  fmap _ AnnotStart     = AnnotStart
  fmap _ (NoAnnot d i)  = NoAnnot d i
  fmap f (AnnotEnd a)   = AnnotEnd (f a)

-- NOTE: Annotations are assumed to have zero length; only text has a length.
annotSize :: AnnotDetails string a -> Int
annotSize (NoAnnot _ l) = l
annotSize _             = 0

-- | A TextDetails represents a fragment of text that will be output at some
-- point in a @Doc@.
data TextDetails string
    = Chr  {-# UNPACK #-} !Char -- ^ A single Char fragment
    | Str  string -- ^ A whole String fragment
    | PStr string -- ^ Used to represent a Fast String fragment
                  --   but now deprecated and identical to the
                  --   Str constructor.
#if __GLASGOW_HASKELL__ >= 701
                 deriving (Show, Eq, Generic)
#endif

-- Combining @Doc@ values
#if __GLASGOW_HASKELL__ >= 800
instance Chars string => Semi.Semigroup (Doc string a) where
#ifndef TESTING
    (<>) = (Text.PrettyPrint.Annotated.HughesPJ.<>)
#else
    (<>) = (PrettyTestVersion.<>)
#endif

instance Chars string => Monoid (Doc string a) where
    mempty  = empty
    mappend = (Semi.<>)
#else
instance Monoid (Doc string a) where
    mempty  = empty
    mappend = (<>)
#endif

instance Chars string => IsString (Doc string a) where
    fromString = text . fromList

instance Chars string => Show (Doc string a) where
  showsPrec _ doc cont =
             toString $ fullRender (mode style) (lineLength style)
                                    (ribbonsPerLine style)
                                    txtPrinter (fromList cont) doc

instance (Chars string, Eq string) => Eq (Doc string a) where
  (==) = (==) `on` render

instance Functor (Doc string) where
  fmap _ Empty               = Empty
  fmap f (NilAbove d)        = NilAbove (fmap f d)
  fmap f (TextBeside td d)   = TextBeside (fmap f td) (fmap f d)
  fmap f (Nest k d)          = Nest k (fmap f d)
  fmap f (Union ur ul)       = Union (fmap f ur) (fmap f ul)
  fmap _ NoDoc               = NoDoc
  fmap f (Beside ld s rd)    = Beside (fmap f ld) s (fmap f rd)
  fmap f (Above ud s ld)     = Above (fmap f ud) s (fmap f ld)

instance (NFData string, NFData a, Chars string) => NFData (Doc string a) where
  rnf Empty               = ()
  rnf (NilAbove d)        = rnf d
  rnf (TextBeside td d)   = rnf td `seq` rnf d
  rnf (Nest k d)          = rnf k  `seq` rnf d
  rnf (Union ur ul)       = rnf ur `seq` rnf ul
  rnf NoDoc               = ()
  rnf (Beside ld s rd)    = rnf ld `seq` rnf s `seq` rnf rd
  rnf (Above ud s ld)     = rnf ud `seq` rnf s `seq` rnf ld

instance (NFData string, NFData a, Chars string) => NFData (AnnotDetails string a) where
  rnf AnnotStart     = ()
  rnf (NoAnnot d sl) = rnf d `seq` rnf sl
  rnf (AnnotEnd a)   = rnf a

instance NFData string => NFData (TextDetails string) where
  rnf (Chr c)    = rnf c
  rnf (Str str)  = rnf str
  rnf (PStr str) = rnf str

-- ---------------------------------------------------------------------------
-- Values and Predicates on GDocs and TextDetails

-- | Attach an annotation to a document.
annotate :: Chars string => a -> Doc string a -> Doc string a
annotate a d = TextBeside AnnotStart
             $ beside (reduceDoc d) False
             $ TextBeside (AnnotEnd a) Empty


-- | A document of height and width 1, containing a literal character.
char :: Chars string => Char -> Doc string a
char c = textBeside_ (NoAnnot (Chr c) 1) Empty

-- | A document of height 1 containing a literal string.
-- 'text' satisfies the following laws:
--
-- * @'text' s '<>' 'text' t = 'text' (s'++'t)@
--
-- * @'text' \"\" '<>' x = x@, if @x@ non-empty
--
-- The side condition on the last law is necessary because @'text' \"\"@
-- has height 1, while 'empty' has no height.
text :: Chars string => string -> Doc string a
text s = case length s of {sl -> textBeside_ (NoAnnot (Str s) sl) Empty}

-- | Same as @text@. Used to be used for Bytestrings.
ptext :: Chars string => string -> Doc string a
ptext s = case length s of {sl -> textBeside_ (NoAnnot (PStr s) sl) Empty}

-- | Some text with any width. (@text s = sizedText (length s) s@)
sizedText :: Chars string => Int -> string -> Doc string a
sizedText l s = textBeside_ (NoAnnot (Str s) l) Empty

-- | Some text, but without any width. Use for non-printing text
-- such as a HTML or Latex tags
zeroWidthText :: Chars string => string -> Doc string a
zeroWidthText = sizedText 0

-- | The empty document, with no height and no width.
-- 'empty' is the identity for '<>', '<+>', '$$' and '$+$', and anywhere
-- in the argument list for 'sep', 'hcat', 'hsep', 'vcat', 'fcat' etc.
empty :: Doc string a
empty = Empty

-- | Returns 'True' if the document is empty
isEmpty :: Doc string a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | Produce spacing for indenting the amount specified.
--
-- an old version inserted tabs being 8 columns apart in the output.
indent :: Chars string => Int -> string
indent !n = fromList $ replicate n ' '

{-
Q: What is the reason for negative indentation (i.e. argument to indent
   is < 0) ?

A:
This indicates an error in the library client's code.
If we compose a <> b, and the first line of b is more indented than some
other lines of b, the law <n6> (<> eats nests) may cause the pretty
printer to produce an invalid layout:

doc       |0123345
------------------
d1        |a...|
d2        |...b|
          |c...|

d1<>d2    |ab..|
         c|....|

Consider a <> b, let `s' be the length of the last line of `a', `k' the
indentation of the first line of b, and `k0' the indentation of the
left-most line b_i of b.

The produced layout will have negative indentation if `k - k0 > s', as
the first line of b will be put on the (s+1)th column, effectively
translating b horizontally by (k-s). Now if the i^th line of b has an
indentation k0 < (k-s), it is translated out-of-page, causing
`negative indentation'.
-}


semi   :: Chars string => Doc string a -- ^ A ';' character
comma  :: Chars string => Doc string a -- ^ A ',' character
colon  :: Chars string => Doc string a -- ^ A ':' character
space  :: Chars string => Doc string a -- ^ A space character
equals :: Chars string => Doc string a -- ^ A '=' character
lparen :: Chars string => Doc string a -- ^ A '(' character
rparen :: Chars string => Doc string a -- ^ A ')' character
lbrack :: Chars string => Doc string a -- ^ A '[' character
rbrack :: Chars string => Doc string a -- ^ A ']' character
lbrace :: Chars string => Doc string a -- ^ A '{' character
rbrace :: Chars string => Doc string a -- ^ A '}' character
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

spaceText, nlText :: AnnotDetails string a
spaceText = NoAnnot (Chr ' ') 1
nlText    = NoAnnot (Chr '\n') 1

int      :: Chars string => Int      -> Doc string a -- ^ @int n = text (show n)@
integer  :: Chars string => Integer  -> Doc string a -- ^ @integer n = text (show n)@
float    :: Chars string => Float    -> Doc string a -- ^ @float n = text (show n)@
double   :: Chars string => Double   -> Doc string a -- ^ @double n = text (show n)@
rational :: Chars string => Rational -> Doc string a -- ^ @rational n = text (show n)@
int      n = text (fromList $ show n)
integer  n = text (fromList $ show n)
float    n = text (fromList $ show n)
double   n = text (fromList $ show n)
rational n = text (fromList $ show n)

parens       :: Chars string => Doc string a -> Doc string a -- ^ Wrap document in @(...)@
brackets     :: Chars string => Doc string a -> Doc string a -- ^ Wrap document in @[...]@
braces       :: Chars string => Doc string a -> Doc string a -- ^ Wrap document in @{...}@
quotes       :: Chars string => Doc string a -> Doc string a -- ^ Wrap document in @\'...\'@
doubleQuotes :: Chars string => Doc string a -> Doc string a -- ^ Wrap document in @\"...\"@
quotes p       = char '\'' <> p <> char '\''
doubleQuotes p = char '"' <> p <> char '"'
parens p       = char '(' <> p <> char ')'
brackets p     = char '[' <> p <> char ']'
braces p       = char '{' <> p <> char '}'

-- | Apply 'parens' to 'Doc' if boolean is true.
maybeParens :: Chars string => Bool -> Doc string a -> Doc string a
maybeParens False = id
maybeParens True = parens

-- | Apply 'brackets' to 'Doc' if boolean is true.
maybeBrackets :: Chars string => Bool -> Doc string a -> Doc string a
maybeBrackets False = id
maybeBrackets True = brackets

-- | Apply 'braces' to 'Doc' if boolean is true.
maybeBraces :: Chars string => Bool -> Doc string a -> Doc string a
maybeBraces False = id
maybeBraces True = braces

-- | Apply 'quotes' to 'Doc' if boolean is true.
maybeQuotes :: Chars string => Bool -> Doc string a -> Doc string a
maybeQuotes False = id
maybeQuotes True = quotes

-- | Apply 'doubleQuotes' to 'Doc' if boolean is true.
maybeDoubleQuotes :: Chars string => Bool -> Doc string a -> Doc string a
maybeDoubleQuotes False = id
maybeDoubleQuotes True = doubleQuotes

-- ---------------------------------------------------------------------------
-- Structural operations on GDocs

-- | Perform some simplification of a built up @GDoc@.
reduceDoc :: Chars string => Doc string a -> RDoc string a
reduceDoc (Beside p g q) = beside p g (reduceDoc q)
reduceDoc (Above  p g q) = above  p g (reduceDoc q)
reduceDoc p              = p

-- | List version of '<>'.
hcat :: Chars string => [Doc string a] -> Doc string a
hcat = snd . reduceHoriz . foldr (\p q -> Beside p False q) empty

-- | List version of '<+>'.
hsep :: Chars string => [Doc string a] -> Doc string a
hsep = snd . reduceHoriz . foldr (\p q -> Beside p True q)  empty

-- | List version of '$$'.
vcat :: Chars string => [Doc string a] -> Doc string a
vcat = snd . reduceVert . foldr (\p q -> Above p False q) empty

-- | Nest (or indent) a document by a given number of positions
-- (which may also be negative).  'nest' satisfies the laws:
--
-- * @'nest' 0 x = x@
--
-- * @'nest' k ('nest' k' x) = 'nest' (k+k') x@
--
-- * @'nest' k (x '<>' y) = 'nest' k z '<>' 'nest' k y@
--
-- * @'nest' k (x '$$' y) = 'nest' k x '$$' 'nest' k y@
--
-- * @'nest' k 'empty' = 'empty'@
--
-- * @x '<>' 'nest' k y = x '<>' y@, if @x@ non-empty
--
-- The side condition on the last law is needed because
-- 'empty' is a left identity for '<>'.
nest :: Chars string => Int -> Doc string a -> Doc string a
nest k p = mkNest k (reduceDoc p)

-- | @hang d1 n d2 = sep [d1, nest n d2]@
hang :: Chars string => Doc string a -> Int -> Doc string a -> Doc string a
hang d1 n d2 = sep [d1, nest n d2]

-- | @punctuate p [d1, ... dn] = [d1 \<> p, d2 \<> p, ... dn-1 \<> p, dn]@
punctuate :: Doc string a -> [Doc string a] -> [Doc string a]
punctuate _ []     = []
punctuate p (x:xs) = go x xs
                   where go y []     = [y]
                         go y (z:zs) = (y <> p) : go z zs

-- mkNest checks for Nest's invariant that it doesn't have an Empty inside it
mkNest :: Chars string => Int -> Doc string a -> Doc string a
mkNest k _ | k `seq` False = undefined
mkNest k (Nest k1 p)       = mkNest (k + k1) p
mkNest _ NoDoc             = NoDoc
mkNest _ Empty             = Empty
mkNest 0 p                 = p
mkNest k p                 = nest_ k p

-- mkUnion checks for an empty document
mkUnion :: Chars string => Doc string a -> Doc string a -> Doc string a
mkUnion Empty _ = Empty
mkUnion p q     = p `union_` q

data IsEmpty = IsEmpty | NotEmpty

reduceHoriz :: Chars string => Doc string a -> (IsEmpty, Doc string a)
reduceHoriz (Beside p g q) = eliminateEmpty Beside (snd (reduceHoriz p)) g (reduceHoriz q)
reduceHoriz doc            = (NotEmpty, doc)

reduceVert :: Chars string => Doc string a -> (IsEmpty, Doc string a)
reduceVert (Above  p g q) = eliminateEmpty Above  (snd (reduceVert p)) g (reduceVert q)
reduceVert doc            = (NotEmpty, doc)

{-# INLINE eliminateEmpty #-}
eliminateEmpty ::
  (Doc string a -> Bool -> Doc string a -> Doc string a) ->
  Doc string a -> Bool -> (IsEmpty, Doc string a) -> (IsEmpty, Doc string a)
eliminateEmpty _    Empty _ q          = q
eliminateEmpty constr p     g q          =
  (NotEmpty,
   -- We're not empty whether or not q is empty, so for laziness-sake,
   -- after checking that p isn't empty, we put the NotEmpty result
   -- outside independent of q. This allows reduceAB to immediately
   -- return the appropriate constructor (Above or Beside) without
   -- forcing the entire nested Doc. This allows the foldr in vcat,
   -- hsep, and hcat to be lazy on its second argument, avoiding a
   -- stack overflow.
   case q of
     (NotEmpty, q') -> constr p g q'
     (IsEmpty, _) -> p)

nilAbove_ :: RDoc string a -> RDoc string a
nilAbove_ = NilAbove

-- | Arg of a TextBeside is always an RDoc.
textBeside_ :: AnnotDetails string a -> RDoc string a -> RDoc string a
textBeside_  = TextBeside

nest_ :: Int -> RDoc string a -> RDoc string a
nest_ = Nest

union_ :: RDoc string a -> RDoc string a -> RDoc string a
union_ = Union


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
($$) :: Doc string a -> Doc string a -> Doc string a
p $$  q = above_ p False q

-- | Above, with no overlapping.
-- '$+$' is associative, with identity 'empty'.
($+$) :: Doc string a -> Doc string a -> Doc string a
p $+$ q = above_ p True q

above_ :: Doc string a -> Bool -> Doc string a -> Doc string a
above_ p _ Empty = p
above_ Empty _ q = q
above_ p g q     = Above p g q

above :: Chars string => Doc string a -> Bool -> RDoc string a -> RDoc string a
above (Above p g1 q1)  g2 q2 = above p g1 (above q1 g2 q2)
above p@(Beside{})     g  q  = aboveNest (reduceDoc p) g 0 (reduceDoc q)
above p g q                  = aboveNest p             g 0 (reduceDoc q)

-- Specfication: aboveNest p g k q = p $g$ (nest k q)
aboveNest :: Chars string => RDoc string a -> Bool -> Int -> RDoc string a -> RDoc string a
aboveNest _                   _ k _ | k `seq` False = undefined
aboveNest NoDoc               _ _ _ = NoDoc
aboveNest (p1 `Union` p2)     g k q = aboveNest p1 g k q `union_`
                                      aboveNest p2 g k q

aboveNest Empty               _ k q = mkNest k q
aboveNest (Nest k1 p)         g k q = nest_ k1 (aboveNest p g (k - k1) q)
                                  -- p can't be Empty, so no need for mkNest

aboveNest (NilAbove p)        g k q = nilAbove_ (aboveNest p g k q)
aboveNest (TextBeside s p)    g k q = TextBeside s rest
                                    where
                                      !k1  = k - annotSize s
                                      rest = case p of
                                                Empty -> nilAboveNest g k1 q
                                                _     -> aboveNest  p g k1 q

aboveNest (Above {})          _ _ _ = error "aboveNest Above"
aboveNest (Beside {})         _ _ _ = error "aboveNest Beside"

-- Specification: text s <> nilaboveNest g k q
--              = text s <> (text "" $g$ nest k q)
nilAboveNest :: Chars string => Bool -> Int -> RDoc string a -> RDoc string a
nilAboveNest _ k _           | k `seq` False = undefined
nilAboveNest _ _ Empty       = Empty
                               -- Here's why the "text s <>" is in the spec!
nilAboveNest g k (Nest k1 q) = nilAboveNest g (k + k1) q
nilAboveNest g k q           | not g && k > 0      -- No newline if no overlap
                             = textBeside_ (NoAnnot (Str (indent k)) k) q
                             | otherwise           -- Put them really above
                             = nilAbove_ (mkNest k q)


-- ---------------------------------------------------------------------------
-- Horizontal composition @<>@

-- We intentionally avoid Data.Monoid.(<>) here due to interactions of
-- Data.Monoid.(<>) and (<+>).  See
-- http://www.haskell.org/pipermail/libraries/2011-November/017066.html

-- | Beside.
-- '<>' is associative, with identity 'empty'.
(<>) :: Doc string a -> Doc string a -> Doc string a
p <>  q = beside_ p False q

-- | Beside, separated by space, unless one of the arguments is 'empty'.
-- '<+>' is associative, with identity 'empty'.
(<+>) :: Doc string a -> Doc string a -> Doc string a
p <+> q = beside_ p True  q

beside_ :: Doc string a -> Bool -> Doc string a -> Doc string a
beside_ p _ Empty = p
beside_ Empty _ q = q
beside_ p g q     = Beside p g q

-- Specification: beside g p q = p <g> q
beside :: Chars string => Doc string a -> Bool -> RDoc string a -> RDoc string a
beside NoDoc               _ _   = NoDoc
beside (p1 `Union` p2)     g q   = beside p1 g q `union_` beside p2 g q
beside Empty               _ q   = q
beside (Nest k p)          g q   = nest_ k $! beside p g q
beside p@(Beside p1 g1 q1) g2 q2
         | g1 == g2              = beside p1 g1 $! beside q1 g2 q2
         | otherwise             = beside (reduceDoc p) g2 q2
beside p@(Above{})         g q   = let !d = reduceDoc p in beside d g q
beside (NilAbove p)        g q   = nilAbove_ $! beside p g q
beside (TextBeside t p)    g q   = TextBeside t rest
                               where
                                  rest = case p of
                                           Empty -> nilBeside g q
                                           _     -> beside p g q

-- Specification: text "" <> nilBeside g p
--              = text "" <g> p
nilBeside :: Chars string => Bool -> RDoc string a -> RDoc string a
nilBeside _ Empty         = Empty -- Hence the text "" in the spec
nilBeside g (Nest _ p)    = nilBeside g p
nilBeside g p | g         = textBeside_ spaceText p
              | otherwise = p


-- ---------------------------------------------------------------------------
-- Separate, @sep@

-- Specification: sep ps  = oneLiner (hsep ps)
--                         `union`
--                          vcat ps

-- | Either 'hsep' or 'vcat'.
sep  :: Chars string => [Doc string a] -> Doc string a
sep = sepX True   -- Separate with spaces

-- | Either 'hcat' or 'vcat'.
cat :: Chars string => [Doc string a] -> Doc string a
cat = sepX False  -- Don't

sepX :: Chars string => Bool -> [Doc string a] -> Doc string a
sepX _ []     = empty
sepX x (p:ps) = sep1 x (reduceDoc p) 0 ps


-- Specification: sep1 g k ys = sep (x : map (nest k) ys)
--                            = oneLiner (x <g> nest k (hsep ys))
--                              `union` x $$ nest k (vcat ys)
sep1 :: Chars string => Bool -> RDoc string a -> Int -> [Doc string a] -> RDoc string a
sep1 _ _                   k _  | k `seq` False = undefined
sep1 _ NoDoc               _ _  = NoDoc
sep1 g (p `Union` q)       k ys = sep1 g p k ys `union_`
                                  aboveNest q False k (reduceDoc (vcat ys))

sep1 g Empty               k ys = mkNest k (sepX g ys)
sep1 g (Nest n p)          k ys = nest_ n (sep1 g p (k - n) ys)

sep1 _ (NilAbove p)        k ys = nilAbove_
                                  (aboveNest p False k (reduceDoc (vcat ys)))
sep1 g (TextBeside s p) k ys    = textBeside_ s (sepNB g p (k - annotSize s) ys)
sep1 _ (Above {})          _ _  = error "sep1 Above"
sep1 _ (Beside {})         _ _  = error "sep1 Beside"

-- Specification: sepNB p k ys = sep1 (text "" <> p) k ys
-- Called when we have already found some text in the first item
-- We have to eat up nests
sepNB :: Chars string => Bool -> Doc string a -> Int -> [Doc string a] -> Doc string a
sepNB g (Nest _ p) k ys
  = sepNB g p k ys -- Never triggered, because of invariant (2)
sepNB g Empty k ys
  = oneLiner (nilBeside g (reduceDoc rest)) `mkUnion`
    -- XXX: TODO: PRETTY: Used to use True here (but GHC used False...)
    nilAboveNest False k (reduceDoc (vcat ys))
  where
    rest | g         = hsep ys
         | otherwise = hcat ys
sepNB g p k ys
  = sep1 g p k ys


-- ---------------------------------------------------------------------------
-- @fill@

-- | \"Paragraph fill\" version of 'cat'.
fcat :: Chars string => [Doc string a] -> Doc string a
fcat = fill False

-- | \"Paragraph fill\" version of 'sep'.
fsep :: Chars string => [Doc string a] -> Doc string a
fsep = fill True

-- Specification:
--
-- fill g docs = fillIndent 0 docs
--
-- fillIndent k [] = []
-- fillIndent k [p] = p
-- fillIndent k (p1:p2:ps) =
--    oneLiner p1 <g> fillIndent (k + length p1 + g ? 1 : 0)
--                               (remove_nests (oneLiner p2) : ps)
--     `Union`
--    (p1 $*$ nest (-k) (fillIndent 0 ps))
--
-- $*$ is defined for layouts (not Docs) as
-- layout1 $*$ layout2 | hasMoreThanOneLine layout1 = layout1 $$ layout2
--                     | otherwise                  = layout1 $+$ layout2

fill :: Chars string => Bool -> [Doc string a] -> RDoc string a
fill _ []     = empty
fill g (p:ps) = fill1 g (reduceDoc p) 0 ps

fill1 :: Chars string => Bool -> RDoc string a -> Int -> [Doc string a] -> Doc string a
fill1 _ _                   k _  | k `seq` False = undefined
fill1 _ NoDoc               _ _  = NoDoc
fill1 g (p `Union` q)       k ys = fill1 g p k ys `union_`
                                   aboveNest q False k (fill g ys)
fill1 g Empty               k ys = mkNest k (fill g ys)
fill1 g (Nest n p)          k ys = nest_ n (fill1 g p (k - n) ys)
fill1 g (NilAbove p)        k ys = nilAbove_ (aboveNest p False k (fill g ys))
fill1 g (TextBeside s p)    k ys = textBeside_ s (fillNB g p (k - annotSize s) ys)
fill1 _ (Above {})          _ _  = error "fill1 Above"
fill1 _ (Beside {})         _ _  = error "fill1 Beside"

fillNB :: Chars string => Bool -> Doc string a -> Int -> [Doc string a] -> Doc string a
fillNB _ _           k _  | k `seq` False = undefined
fillNB g (Nest _ p)  k ys   = fillNB g p k ys
                              -- Never triggered, because of invariant (2)
fillNB _ Empty _ []         = Empty
fillNB g Empty k (Empty:ys) = fillNB g Empty k ys
fillNB g Empty k (y:ys)     = fillNBE g k y ys
fillNB g p k ys             = fill1 g p k ys


fillNBE :: Chars string => Bool -> Int -> Doc string a -> [Doc string a] -> Doc string a
fillNBE g k y ys
  = nilBeside g (fill1 g ((elideNest . oneLiner . reduceDoc) y) k' ys)
    -- XXX: TODO: PRETTY: Used to use True here (but GHC used False...)
    `mkUnion` nilAboveNest False k (fill g (y:ys))
  where k' = if g then k - 1 else k

elideNest :: Doc string a -> Doc string a
elideNest (Nest _ d) = d
elideNest d          = d


-- ---------------------------------------------------------------------------
-- Selecting the best layout

best :: Chars string
     => Int   -- Line length.
     -> Int   -- Ribbon length.
     -> RDoc string a
     -> RDoc string a  -- No unions in here!.
best w0 r = get w0
  where
    get w _ | w == 0 && False = undefined
    get _ Empty               = Empty
    get _ NoDoc               = NoDoc
    get w (NilAbove p)        = nilAbove_ (get w p)
    get w (TextBeside s p)    = textBeside_ s (get1 w (annotSize s) p)
    get w (Nest k p)          = nest_ k (get (w - k) p)
    get w (p `Union` q)       = nicest w r (get w p) (get w q)
    get _ (Above {})          = error "best get Above"
    get _ (Beside {})         = error "best get Beside"

    get1 w _ _ | w == 0 && False  = undefined
    get1 _ _  Empty               = Empty
    get1 _ _  NoDoc               = NoDoc
    get1 w sl (NilAbove p)        = nilAbove_ (get (w - sl) p)
    get1 w sl (TextBeside s p)    = textBeside_ s (get1 w (sl + annotSize s) p)
    get1 w sl (Nest _ p)          = get1 w sl p
    get1 w sl (p `Union` q)       = nicest1 w r sl (get1 w sl p)
                                                   (get1 w sl q)
    get1 _ _  (Above {})          = error "best get1 Above"
    get1 _ _  (Beside {})         = error "best get1 Beside"

nicest :: Chars string => Int -> Int -> Doc string a -> Doc string a -> Doc string a
nicest !w !r = nicest1 w r 0

nicest1 :: Chars string => Int -> Int -> Int -> Doc string a -> Doc string a -> Doc string a
nicest1 !w !r !sl p q | fits ((w `min` r) - sl) p = p
                      | otherwise                 = q

fits :: Chars string
     => Int  -- Space available
     -> Doc string a
     -> Bool -- True if *first line* of Doc fits in space available
fits n _ | n < 0           = False
fits _ NoDoc               = False
fits _ Empty               = True
fits _ (NilAbove _)        = True
fits n (TextBeside s p)    = fits (n - annotSize s) p
fits _ (Above {})          = error "fits Above"
fits _ (Beside {})         = error "fits Beside"
fits _ (Union {})          = error "fits Union"
fits _ (Nest {})           = error "fits Nest"

-- | @first@ returns its first argument if it is non-empty, otherwise its
-- second.
first :: Chars string => Doc string a -> Doc string a -> Doc string a
first p q | nonEmptySet p = p -- unused, because (get OneLineMode) is unused
          | otherwise     = q

nonEmptySet :: Chars string => Doc string a -> Bool
nonEmptySet NoDoc              = False
nonEmptySet (_ `Union` _)      = True
nonEmptySet Empty              = True
nonEmptySet (NilAbove _)       = True
nonEmptySet (TextBeside _ p)   = nonEmptySet p
nonEmptySet (Nest _ p)         = nonEmptySet p
nonEmptySet (Above {})         = error "nonEmptySet Above"
nonEmptySet (Beside {})        = error "nonEmptySet Beside"

-- @oneLiner@ returns the one-line members of the given set of @GDoc@s.
oneLiner :: Chars string => Doc string a -> Doc string a
oneLiner NoDoc               = NoDoc
oneLiner Empty               = Empty
oneLiner (NilAbove _)        = NoDoc
oneLiner (TextBeside s p)    = textBeside_ s (oneLiner p)
oneLiner (Nest k p)          = nest_ k (oneLiner p)
oneLiner (p `Union` _)       = oneLiner p
oneLiner (Above {})          = error "oneLiner Above"
oneLiner (Beside {})         = error "oneLiner Beside"


-- ---------------------------------------------------------------------------
-- Rendering

-- | A rendering style. Allows us to specify constraints to choose among the
-- many different rendering options.
data Style
  = Style { mode           :: Mode
            -- ^ The rendering mode.
          , lineLength     :: Int
            -- ^ Maximum length of a line, in characters.
          , ribbonsPerLine :: Float
            -- ^ Ratio of line length to ribbon length. A ribbon refers to the
            -- characters on a line /excluding/ indentation. So a 'lineLength'
            -- of 100, with a 'ribbonsPerLine' of @2.0@ would only allow up to
            -- 50 characters of ribbon to be displayed on a line, while
            -- allowing it to be indented up to 50 characters.
          }
#if __GLASGOW_HASKELL__ >= 701
  deriving (Show, Eq, Generic)
#endif

-- | The default style (@mode=PageMode, lineLength=100, ribbonsPerLine=1.5@).
style :: Style
style = Style { lineLength = 100, ribbonsPerLine = 1.5, mode = PageMode }

-- | Rendering mode.
data Mode = PageMode    
            -- ^ Normal rendering ('lineLength' and 'ribbonsPerLine'
            -- respected').
          | ZigZagMode  
            -- ^ With zig-zag cuts.
          | LeftMode    
            -- ^ No indentation, infinitely long lines ('lineLength' ignored),
            -- but explicit new lines, i.e., @text "one" $$ text "two"@, are
            -- respected.
          | OneLineMode 
            -- ^ All on one line, 'lineLength' ignored and explicit new lines
            -- (@$$@) are turned into spaces.
#if __GLASGOW_HASKELL__ >= 701
          deriving (Show, Eq, Generic)
#endif

-- | Render the @Doc@ to string a using the default @Style@ (see 'style').
render :: Chars string => Doc string a -> string
render = fullRender (mode style) (lineLength style) (ribbonsPerLine style)
                    txtPrinter ""

-- | Render the @Doc@ to string a using the given @Style@.
renderStyle :: Chars string => Style -> Doc string a -> string
renderStyle s = fullRender (mode s) (lineLength s) (ribbonsPerLine s)
                txtPrinter ""

-- | Default TextDetails printer.
txtPrinter :: Chars string => TextDetails string -> string -> string
txtPrinter (Chr c)   s  = cons c s
txtPrinter (Str s1)  s2 = s1 `mappend` s2
txtPrinter (PStr s1) s2 = s1 `mappend` s2

-- | The general rendering interface. Please refer to the @Style@ and @Mode@
-- types for a description of rendering mode, line length and ribbons.
fullRender :: Chars string
           => Mode                    -- ^ Rendering mode.
           -> Int                     -- ^ Line length.
           -> Float                   -- ^ Ribbons per line.
           -> (TextDetails string -> a -> a) -- ^ What to do with text.
           -> a                       -- ^ What to do at the end.
           -> Doc string b            -- ^ The document.
           -> a                       -- ^ Result.
fullRender m l r txt = fullRenderAnn m l r annTxt
  where
  annTxt (NoAnnot s _) = txt s
  annTxt _             = id

-- | The general rendering interface, supporting annotations. Please refer to
-- the @Style@ and @Mode@ types for a description of rendering mode, line
-- length and ribbons.
fullRenderAnn :: Chars string
              => Mode                       -- ^ Rendering mode.
              -> Int                        -- ^ Line length.
              -> Float                      -- ^ Ribbons per line.
              -> (AnnotDetails string b -> a -> a) -- ^ What to do with text.
              -> a                          -- ^ What to do at the end.
              -> Doc string b               -- ^ The document.
              -> a                          -- ^ Result.
fullRenderAnn OneLineMode _ _ txt end doc
  = easyDisplay spaceText (\_ y -> y) txt end (reduceDoc doc)
fullRenderAnn LeftMode    _ _ txt end doc
  = easyDisplay nlText first txt end (reduceDoc doc)

fullRenderAnn m lineLen ribbons txt rest doc
  = display m lineLen ribbonLen txt rest doc'
  where
    doc' = best bestLineLen ribbonLen (reduceDoc doc)

    bestLineLen, ribbonLen :: Int
    ribbonLen   = round (fromIntegral lineLen / ribbons)
    bestLineLen = case m of
                      ZigZagMode -> maxBound
                      _          -> lineLen

easyDisplay :: AnnotDetails string b
             -> (Doc string b -> Doc string b -> Doc string b)
             -> (AnnotDetails string b -> a -> a)
             -> a
             -> Doc string b
             -> a
easyDisplay nlSpaceText choose txt end
  = lay
  where
    lay NoDoc              = error "easyDisplay: NoDoc"
    lay (Union p q)        = lay (choose p q)
    lay (Nest _ p)         = lay p
    lay Empty              = end
    lay (NilAbove p)       = nlSpaceText `txt` lay p
    lay (TextBeside s p)   = s `txt` lay p
    lay (Above {})         = error "easyDisplay Above"
    lay (Beside {})        = error "easyDisplay Beside"

display :: Chars string => Mode -> Int -> Int -> (AnnotDetails string b -> a -> a) -> a -> Doc string b -> a
display m !page_width !ribbon_width txt end doc
  = case page_width - ribbon_width of { gap_width ->
    case gap_width `quot` 2 of { shift ->
    let
        lay k _            | k `seq` False = undefined
        lay k (Nest k1 p)  = lay (k + k1) p
        lay _ Empty        = end
        lay k (NilAbove p) = nlText `txt` lay k p
        lay k (TextBeside s p)
            = case m of
                    ZigZagMode |  k >= gap_width
                               -> nlText `txt` (
                                  NoAnnot (Str (fromList (replicate shift '/'))) shift `txt` (
                                  nlText `txt`
                                  lay1 (k - shift) s p ))

                               |  k < 0
                               -> nlText `txt` (
                                  NoAnnot (Str (fromList (replicate shift '\\'))) shift `txt` (
                                  nlText `txt`
                                  lay1 (k + shift) s p ))

                    _ -> lay1 k s p

        lay _ (Above {})   = error "display lay Above"
        lay _ (Beside {})  = error "display lay Beside"
        lay _ NoDoc        = error "display lay NoDoc"
        lay _ (Union {})   = error "display lay Union"

        lay1 !k s p        = let !r = k + annotSize s
                             in NoAnnot (Str (indent k)) k `txt` (s `txt` lay2 r p)

        lay2 k _ | k `seq` False   = undefined
        lay2 k (NilAbove p)        = nlText `txt` lay k p
        lay2 k (TextBeside s p)    = s `txt` lay2 (k + annotSize s) p
        lay2 k (Nest _ p)          = lay2 k p
        lay2 _ Empty               = end
        lay2 _ (Above {})          = error "display lay2 Above"
        lay2 _ (Beside {})         = error "display lay2 Beside"
        lay2 _ NoDoc               = error "display lay2 NoDoc"
        lay2 _ (Union {})          = error "display lay2 Union"
    in
    lay 0 doc
    }}



-- Rendering Annotations -------------------------------------------------------

-- | A @Span@ represents the result of an annotation after a @Doc@ has been
-- rendered, capturing where the annotation now starts and ends in the rendered
-- output.
data Span a = Span { spanStart      :: !Int
                   , spanLength     :: !Int
                   , spanAnnotation :: a
                   } deriving (Show,Eq)

instance Functor Span where
  fmap f (Span x y a) = Span x y (f a)


-- State required for generating document spans.
data Spans string a =
    Spans { sOffset :: !Int
          -- ^ Current offset from the end of the document.
          , sStack  :: [Int -> Span a]
          -- ^ Currently open spans.
          , sSpans  :: [Span a]
          -- ^ Collected annotation regions.
          , sOutput :: string
          -- ^ Collected output.
          }

-- | Render an annotated @Doc@ to string a and list of annotations (see 'Span')
-- using the default @Style@ (see 'style').
renderSpans :: Chars string => Doc string ann -> (string,[Span ann])
renderSpans  = finalize
             . fullRenderAnn (mode style) (lineLength style) (ribbonsPerLine style)
                  spanPrinter
                  Spans { sOffset = 0, sStack = [], sSpans = [], sOutput = mempty }
  where

  finalize (Spans size _ spans out) = (out, fmap adjust spans)
    where
    adjust s = s { spanStart = size - spanStart s }

  mkSpan a end start = Span { spanStart      = start
                            , spanLength     = start - end
                              -- this seems wrong, but remember that it's
                              -- working backwards at this point
                            , spanAnnotation = a }

  -- the document gets generated in reverse, which is why the starting
  -- annotation ends the annotation.
  spanPrinter :: Chars string => AnnotDetails string a -> Spans string a -> Spans string a
  spanPrinter AnnotStart s =
    case sStack s of
      sp : rest -> s { sSpans = sp (sOffset s) : sSpans s, sStack = rest }
      _         -> error "renderSpans: stack underflow"

  spanPrinter (AnnotEnd a) s =
    s { sStack = mkSpan a (sOffset s) : sStack s }

  spanPrinter (NoAnnot td l) s =
    case td of
      Chr  c -> s { sOutput = cons c  (sOutput s), sOffset = sOffset s + l }
      Str  t -> s { sOutput = t `mappend` sOutput s, sOffset = sOffset s + l }
      PStr t -> s { sOutput = t `mappend` sOutput s, sOffset = sOffset s + l }


-- | Render out string a, interpreting the annotations as part of the resulting
-- document.
--
-- /IMPORTANT/: the size of the annotation string does NOT figure into the
-- layout of the document, so the document will lay out as though the
-- annotations are not present.
renderDecorated :: Chars string =>
                   (ann -> string) -- ^ Starting an annotation.
                -> (ann -> string) -- ^ Ending an annotation.
                -> Doc string ann -> string
renderDecorated startAnn endAnn =
  finalize . fullRenderAnn (mode style) (lineLength style) (ribbonsPerLine style)
                 annPrinter
                 (mempty, mempty)
  where
  annPrinter AnnotStart (rest,stack) =
    case stack of
      a : as -> (startAnn a `mappend` rest, as)
      _      -> error "renderDecorated: stack underflow"

  annPrinter (AnnotEnd a) (rest,stack) =
    (endAnn a `mappend` rest, a : stack)

  annPrinter (NoAnnot s _) (rest,stack) =
    (txtPrinter s rest, stack)

  finalize (str,_) = str


-- | Render a document with annotations, by interpreting the start and end of
-- the annotations, as well as the text details in the context of a monad.
renderDecoratedM :: (Monad m, Chars string)
                 => (ann    -> m r) -- ^ Starting an annotation.
                 -> (ann    -> m r) -- ^ Ending an annotation.
                 -> (string -> m r) -- ^ Text formatting.
                 -> m r             -- ^ Document end.
                 -> Doc string ann -> m r
renderDecoratedM startAnn endAnn txt docEnd =
  finalize . fullRenderAnn (mode style) (lineLength style) (ribbonsPerLine style)
                 annPrinter
                 (docEnd, [])
  where
  annPrinter AnnotStart (rest,stack) =
    case stack of
      a : as -> (startAnn a >> rest, as)
      _      -> error "renderDecorated: stack underflow"

  annPrinter (AnnotEnd a) (rest,stack) =
    (endAnn a >> rest, a : stack)

  annPrinter (NoAnnot td _) (rest,stack) =
    case td of
      Chr  c -> (txt (fromList [c]) >> rest, stack)
      Str  s -> (txt s   >> rest, stack)
      PStr s -> (txt s   >> rest, stack)

  finalize (m,_) = m

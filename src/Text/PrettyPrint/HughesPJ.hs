{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE BangPatterns #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.HughesPJ
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David Terei <dave.terei@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- John Hughes's and Simon Peyton Jones's Pretty Printer Combinators
--
-- Based on /The Design of a Pretty-printing Library/
-- in Advanced Functional Programming,
-- Johan Jeuring and Erik Meijer (eds), LNCS 925
-- <http://www.cs.chalmers.se/~rjmh/Papers/pretty.ps>
--
-----------------------------------------------------------------------------

#ifndef TESTING
module Text.PrettyPrint.HughesPJ (

        -- * The document type
        Doc, DocL, TextDetails(..),

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
        first, reduceDoc,
        -- TODO: Should these be exported? Previously they weren't

        -- * Rendering documents

        -- ** Default rendering
        render,

        -- ** Rendering with a particular style
        Style(..),
        style,
        renderStyle,
        Mode(..),

        -- ** General rendering
        fullRender,

        -- ** Logging
        here,
        Position(..), Logger,
        renderWithLog,
        renderStyleWithLog,
        fullRenderWithLog
    ) where
#endif

import Data.Monoid ( Monoid(mempty, mappend) )
import Data.String ( IsString(fromString) )
import Prelude hiding (log)

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

-- | The abstract type of documents.
-- A Doc represents a *set* of layouts. A Doc with
-- no occurrences of Union or NoDoc represents just one layout.
data DocL m
  = Empty                                                 -- empty
  | NilAbove (DocL m)                                      -- text "" $$ x 
  | TextBeside !TextDetails {-# UNPACK #-} !Int (Maybe (Logger m)) (DocL m) -- text s <> x with optional log entry
  | Nest {-# UNPACK #-} !Int (DocL m)                      -- nest k x
  | Union (DocL m) (DocL m)                                 -- ul `union` ur
  | NoDoc                                                 -- The empty set of documents
  | Beside (DocL m) Bool (DocL m)                           -- True <=> space between
  | Above (DocL m) Bool (DocL m)                            -- True <=> never overlap

type Doc = DocL ()

{-
Here are the invariants:

1) The argument of NilAbove is never Empty. Therefore
   a NilAbove occupies at least two lines.

2) The argument of @TextBeside@ is never @Nest@.

3) The layouts of the two arguments of @Union@ both flatten to the same
   string.

4) The arguments of @Union@ are either @TextBeside@, or @NilAbove@.

5) A @NoDoc@ may only appear on the first line of the left argument of an
   union. Therefore, the right argument of an union can never be equivalent
   to the empty set (@NoDoc@).

6) An empty document is always represented by @Empty@.  It can't be
   hidden inside a @Nest@, or a @Union@ of two @Empty@s.

7) The first line of every layout in the left argument of @Union@ is
   longer than the first line of any layout in the right argument.
   (1) ensures that the left argument has a first line.  In view of
   (3), this invariant means that the right argument must have at
   least two lines.

Notice the difference between
   * NoDoc (no documents)
   * Empty (one empty document; no height and no width)
   * text "" (a document containing the empty string;
              one line high, but has no width)
-}


-- | RDoc is a "reduced GDoc", guaranteed not to have a top-level Above or Beside.
type RDocL m = DocL m

-- | The TextDetails data type
--
-- A TextDetails represents a fragment of text that will be
-- output at some point.
data TextDetails = Chr  {-# UNPACK #-} !Char -- ^ A single Char fragment
                 | Str  String -- ^ A whole String fragment
                 | PStr String -- ^ Used to represent a Fast String fragment
                               --   but now deprecated and identical to the
                               --   Str constructor.

-- Combining @Doc@ values
instance Monoid (DocL m) where
    mempty  = empty
    mappend = (<>)

instance IsString (DocL m) where
    fromString = text

instance Monoid m => Show (DocL m) where
  showsPrec _ doc cont = fst $
                         fullRenderWithLog (mode style) (lineLength style)
                                           (ribbonsPerLine style)
                                           txtPrinter cont doc


-- ---------------------------------------------------------------------------
-- Logging

data Position = Position {row :: !Int, column :: !Int} deriving (Show, Eq)

start :: Position
start = Position 1 1

advance :: Position -> Int -> Position
advance (Position r c) s = Position r (c + s)

newline :: Position -> Position
newline (Position r _) = Position (r + 1) 1

type Logger m = Position -> m

log :: Monoid m => m -> (Maybe (Logger m)) -> Position -> m
log m Nothing _  = m
log m (Just l) w = m `mappend` l w

mergeLogger :: Monoid m => Logger m -> Logger m -> Logger m
mergeLogger l1 l2 = \p -> l1 p `mappend` l2 p

here :: (Monoid m) => Logger m -> DocL m -> DocL m
here _ Empty                         = Empty
here l (NilAbove d)                  = NilAbove (here l d)
here l (TextBeside s sl Nothing p)   = TextBeside s sl (Just l) p
here l (TextBeside s sl (Just l') p) = TextBeside s sl (Just (mergeLogger l l')) p
here l (Nest x d)                    = Nest x (here l d)
here l (Union d1 d2)                 = Union (here l d1) (here l d2)
here _ NoDoc                         = NoDoc
here l (Beside d1 f d2)              = Beside (here l d1) f d2
here l (Above d1 f d2)               = Above (here l d1) f d2

-- ---------------------------------------------------------------------------
-- Values and Predicates on GDocs and TextDetails

-- | A document of height and width 1, containing a literal character.
char :: Char -> DocL m
char c = textBeside_ (Chr c) 1 Nothing Empty

-- | A document of height 1 containing a literal string.
-- 'text' satisfies the following laws:
--
-- * @'text' s '<>' 'text' t = 'text' (s'++'t)@
--
-- * @'text' \"\" '<>' x = x@, if @x@ non-empty
--
-- The side condition on the last law is necessary because @'text' \"\"@
-- has height 1, while 'empty' has no height.
text :: String -> DocL m
text s = case length s of {sl -> textBeside_ (Str s) sl Nothing Empty}

-- | Same as @text@. Used to be used for Bytestrings.
ptext :: String -> DocL m
ptext s = case length s of {sl -> textBeside_ (PStr s) sl Nothing Empty}

-- | Some text with any width. (@text s = sizedText (length s) s@)
sizedText :: Int -> String -> DocL m
sizedText l s = textBeside_ (Str s) l Nothing Empty

-- | Some text, but without any width. Use for non-printing text
-- such as a HTML or Latex tags
zeroWidthText :: String -> DocL m
zeroWidthText = sizedText 0

-- | The empty document, with no height and no width.
-- 'empty' is the identity for '<>', '<+>', '$$' and '$+$', and anywhere
-- in the argument list for 'sep', 'hcat', 'hsep', 'vcat', 'fcat' etc.
empty :: DocL m
empty = Empty

-- | Returns 'True' if the document is empty
isEmpty :: DocL m -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | Produce spacing for indenting the amount specified.
--
-- an old version inserted tabs being 8 columns apart in the output.
indent :: Int -> String
indent !n = replicate n ' '

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


semi   :: DocL m -- ^ A ';' character
comma  :: DocL m -- ^ A ',' character
colon  :: DocL m -- ^ A ':' character
space  :: DocL m -- ^ A space character
equals :: DocL m -- ^ A '=' character
lparen :: DocL m -- ^ A '(' character
rparen :: DocL m -- ^ A ')' character
lbrack :: DocL m -- ^ A '[' character
rbrack :: DocL m -- ^ A ']' character
lbrace :: DocL m -- ^ A '{' character
rbrace :: DocL m -- ^ A '}' character
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

space_text, nl_text :: TextDetails
space_text = Chr ' '
nl_text    = Chr '\n'

int      :: Int      -> DocL m -- ^ @int n = text (show n)@
integer  :: Integer  -> DocL m -- ^ @integer n = text (show n)@
float    :: Float    -> DocL m -- ^ @float n = text (show n)@
double   :: Double   -> DocL m -- ^ @double n = text (show n)@
rational :: Rational -> DocL m -- ^ @rational n = text (show n)@
int      n = text (show n)
integer  n = text (show n)
float    n = text (show n)
double   n = text (show n)
rational n = text (show n)

parens       :: DocL m -> DocL m -- ^ Wrap document in @(...)@
brackets     :: DocL m -> DocL m -- ^ Wrap document in @[...]@
braces       :: DocL m -> DocL m -- ^ Wrap document in @{...}@
quotes       :: DocL m -> DocL m -- ^ Wrap document in @\'...\'@
doubleQuotes :: DocL m -> DocL m -- ^ Wrap document in @\"...\"@
quotes p       = char '\'' <> p <> char '\''
doubleQuotes p = char '"' <> p <> char '"'
parens p       = char '(' <> p <> char ')'
brackets p     = char '[' <> p <> char ']'
braces p       = char '{' <> p <> char '}'


-- ---------------------------------------------------------------------------
-- Structural operations on GDocs

-- | Perform some simplification of a built up @GDoc@.
reduceDoc :: DocL m -> RDocL m
reduceDoc (Beside p g q) = beside p g (reduceDoc q)
reduceDoc (Above  p g q) = above  p g (reduceDoc q)
reduceDoc p              = p

-- | List version of '<>'.
hcat :: [DocL m] -> DocL m
hcat = reduceAB . foldr (beside_' False) empty

-- | List version of '<+>'.
hsep :: [DocL m] -> DocL m
hsep = reduceAB . foldr (beside_' True)  empty

-- | List version of '$$'.
vcat :: [DocL m] -> DocL m
vcat = reduceAB . foldr (above_' False) empty

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
nest :: Int -> DocL m -> DocL m
nest k p = mkNest k (reduceDoc p)

-- | @hang d1 n d2 = sep [d1, nest n d2]@
hang :: DocL m -> Int -> DocL m -> DocL m
hang d1 n d2 = sep [d1, nest n d2]

-- | @punctuate p [d1, ... dn] = [d1 \<> p, d2 \<> p, ... dn-1 \<> p, dn]@
punctuate :: DocL m -> [DocL m] -> [DocL m]
punctuate _ []     = []
punctuate p (x:xs) = go x xs
                   where go y []     = [y]
                         go y (z:zs) = (y <> p) : go z zs

-- mkNest checks for Nest's invariant that it doesn't have an Empty inside it
mkNest :: Int -> DocL m -> DocL m
mkNest k _ | k `seq` False = undefined
mkNest k (Nest k1 p)       = mkNest (k + k1) p
mkNest _ NoDoc             = NoDoc
mkNest _ Empty             = Empty
mkNest 0 p                 = p
mkNest k p                 = nest_ k p

-- mkUnion checks for an empty document
mkUnion :: DocL m -> DocL m -> DocL m
mkUnion Empty _ = Empty
mkUnion p q     = p `union_` q

beside_' :: Bool -> DocL m -> DocL m -> DocL m
beside_' _ p Empty = p
beside_' g p q     = Beside p g q

above_' :: Bool -> DocL m -> DocL m -> DocL m
above_' _ p Empty = p
above_' g p q     = Above p g q

reduceAB :: DocL m -> DocL m
reduceAB (Above  Empty _ q) = q
reduceAB (Beside Empty _ q) = q
reduceAB doc                = doc

nilAbove_ :: RDocL m -> RDocL m
nilAbove_ p = NilAbove p

-- Arg of a TextBeside is always an RDoc
textBeside_ :: TextDetails -> Int -> (Maybe (Logger m)) -> RDocL m -> RDocL m
textBeside_ s sl l p = TextBeside s sl l p

nest_ :: Int -> RDocL m -> RDocL m
nest_ k p = Nest k p

union_ :: RDocL m -> RDocL m -> RDocL m
union_ p q = Union p q


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
($$) :: DocL m -> DocL m -> DocL m
p $$  q = above_ p False q

-- | Above, with no overlapping.
-- '$+$' is associative, with identity 'empty'.
($+$) :: DocL m -> DocL m -> DocL m
p $+$ q = above_ p True q

above_ :: DocL m -> Bool -> DocL m -> DocL m
above_ p _ Empty = p
above_ Empty _ q = q
above_ p g q     = Above p g q

above :: DocL m -> Bool -> RDocL m -> RDocL m
above (Above p g1 q1)  g2 q2 = above p g1 (above q1 g2 q2)
above p@(Beside _ _ _) g  q  = aboveNest (reduceDoc p) g 0 (reduceDoc q)
above p g q                  = aboveNest p             g 0 (reduceDoc q)

-- Specfication: aboveNest p g k q = p $g$ (nest k q)
aboveNest :: RDocL m -> Bool -> Int -> RDocL m -> RDocL m
aboveNest _                   _ k _ | k `seq` False = undefined
aboveNest NoDoc               _ _ _ = NoDoc
aboveNest (p1 `Union` p2)     g k q = aboveNest p1 g k q `union_`
                                      aboveNest p2 g k q

aboveNest Empty               _ k q = mkNest k q
aboveNest (Nest k1 p)         g k q = nest_ k1 (aboveNest p g (k - k1) q)
                                  -- p can't be Empty, so no need for mkNest

aboveNest (NilAbove p)        g k q = nilAbove_ (aboveNest p g k q)
aboveNest (TextBeside s sl l p) g k q = textBeside_ s sl l rest
                                    where
                                      !k1  = k - sl
                                      rest = case p of
                                                Empty -> nilAboveNest g k1 q
                                                _     -> aboveNest  p g k1 q
aboveNest (Above {})          _ _ _ = error "aboveNest Above"
aboveNest (Beside {})         _ _ _ = error "aboveNest Beside"

-- Specification: text s <> nilaboveNest g k q
--              = text s <> (text "" $g$ nest k q)
nilAboveNest :: Bool -> Int -> RDocL m -> RDocL m
nilAboveNest _ k _           | k `seq` False = undefined
nilAboveNest _ _ Empty       = Empty
                               -- Here's why the "text s <>" is in the spec!
nilAboveNest g k (Nest k1 q) = nilAboveNest g (k + k1) q
nilAboveNest g k q           | not g && k > 0      -- No newline if no overlap
                             = textBeside_ (Str (indent k)) k Nothing q
                             | otherwise           -- Put them really above
                             = nilAbove_ (mkNest k q)


-- ---------------------------------------------------------------------------
-- Horizontal composition @<>@

-- We intentionally avoid Data.Monoid.(<>) here due to interactions of
-- Data.Monoid.(<>) and (<+>).  See
-- http://www.haskell.org/pipermail/libraries/2011-November/017066.html

-- | Beside.
-- '<>' is associative, with identity 'empty'.
(<>) :: DocL m -> DocL m -> DocL m
p <>  q = beside_ p False q

-- | Beside, separated by space, unless one of the arguments is 'empty'.
-- '<+>' is associative, with identity 'empty'.
(<+>) :: DocL m -> DocL m -> DocL m
p <+> q = beside_ p True  q

beside_ :: DocL m -> Bool -> DocL m -> DocL m
beside_ p _ Empty = p
beside_ Empty _ q = q
beside_ p g q     = Beside p g q

-- Specification: beside g p q = p <g> q
beside :: DocL m -> Bool -> RDocL m -> RDocL m
beside NoDoc               _ _   = NoDoc
beside (p1 `Union` p2)     g q   = beside p1 g q `union_` beside p2 g q
beside Empty               _ q   = q
beside (Nest k p)          g q   = nest_ k $! beside p g q
beside p@(Beside p1 g1 q1) g2 q2
         | g1 == g2              = beside p1 g1 $! beside q1 g2 q2
         | otherwise             = beside (reduceDoc p) g2 q2
beside p@(Above _ _ _)     g q   = let !d = reduceDoc p in beside d g q
beside (NilAbove p)        g q   = nilAbove_ $! beside p g q
beside (TextBeside s sl l p) g q   = sl `seq` textBeside_ s sl l rest
                               where
                                  rest = case p of
                                           Empty -> nilBeside g q
                                           _     -> beside p g q

-- Specification: text "" <> nilBeside g p
--              = text "" <g> p
nilBeside :: Bool -> RDocL m -> RDocL m
nilBeside _ Empty         = Empty -- Hence the text "" in the spec
nilBeside g (Nest _ p)    = nilBeside g p
nilBeside g p | g         = textBeside_ space_text 1 Nothing p
              | otherwise = p


-- ---------------------------------------------------------------------------
-- Separate, @sep@

-- Specification: sep ps  = oneLiner (hsep ps)
--                         `union`
--                          vcat ps

-- | Either 'hsep' or 'vcat'.
sep  :: [DocL m] -> DocL m
sep = sepX True   -- Separate with spaces

-- | Either 'hcat' or 'vcat'.
cat :: [DocL m] -> DocL m
cat = sepX False  -- Don't

sepX :: Bool -> [DocL m] -> DocL m
sepX _ []     = empty
sepX x (p:ps) = sep1 x (reduceDoc p) 0 ps


-- Specification: sep1 g k ys = sep (x : map (nest k) ys)
--                            = oneLiner (x <g> nest k (hsep ys))
--                              `union` x $$ nest k (vcat ys)
sep1 :: Bool -> RDocL m -> Int -> [DocL m] -> RDocL m
sep1 _ _                   k _  | k `seq` False = undefined
sep1 _ NoDoc               _ _  = NoDoc
sep1 g (p `Union` q)       k ys = sep1 g p k ys `union_`
                                  aboveNest q False k (reduceDoc (vcat ys))

sep1 g Empty               k ys = mkNest k (sepX g ys)
sep1 g (Nest n p)          k ys = nest_ n (sep1 g p (k - n) ys)

sep1 _ (NilAbove p)        k ys = nilAbove_
                                  (aboveNest p False k (reduceDoc (vcat ys)))
sep1 g (TextBeside s sl l p) k ys = textBeside_ s sl l (sepNB g p (k - sl) ys)
sep1 _ (Above {})          _ _  = error "sep1 Above"
sep1 _ (Beside {})         _ _  = error "sep1 Beside"

-- Specification: sepNB p k ys = sep1 (text "" <> p) k ys
-- Called when we have already found some text in the first item
-- We have to eat up nests
sepNB :: Bool -> DocL m -> Int -> [DocL m] -> DocL m
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
fcat :: [DocL m] -> DocL m
fcat = fill False

-- | \"Paragraph fill\" version of 'sep'.
fsep :: [DocL m] -> DocL m
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

fill :: Bool -> [DocL m] -> RDocL m
fill _ []     = empty
fill g (p:ps) = fill1 g (reduceDoc p) 0 ps

fill1 :: Bool -> RDocL m -> Int -> [DocL m] -> DocL m
fill1 _ _                   k _  | k `seq` False = undefined
fill1 _ NoDoc               _ _  = NoDoc
fill1 g (p `Union` q)       k ys = fill1 g p k ys `union_`
                                   aboveNest q False k (fill g ys)
fill1 g Empty               k ys = mkNest k (fill g ys)
fill1 g (Nest n p)          k ys = nest_ n (fill1 g p (k - n) ys)
fill1 g (NilAbove p)        k ys = nilAbove_ (aboveNest p False k (fill g ys))
fill1 g (TextBeside s sl l p) k ys = textBeside_ s sl l (fillNB g p (k - sl) ys)
fill1 _ (Above {})          _ _  = error "fill1 Above"
fill1 _ (Beside {})         _ _  = error "fill1 Beside"

fillNB :: Bool -> DocL m -> Int -> [DocL m] -> DocL m
fillNB _ _           k _  | k `seq` False = undefined
fillNB g (Nest _ p)  k ys   = fillNB g p k ys
                              -- Never triggered, because of invariant (2)
fillNB _ Empty _ []         = Empty
fillNB g Empty k (Empty:ys) = fillNB g Empty k ys
fillNB g Empty k (y:ys)     = fillNBE g k y ys
fillNB g p k ys             = fill1 g p k ys


fillNBE :: Bool -> Int -> DocL m -> [DocL m] -> DocL m
fillNBE g k y ys
  = nilBeside g (fill1 g ((elideNest . oneLiner . reduceDoc) y) k' ys)
    -- XXX: TODO: PRETTY: Used to use True here (but GHC used False...)
    `mkUnion` nilAboveNest False k (fill g (y:ys))
  where k' = if g then k - 1 else k

elideNest :: DocL m -> DocL m
elideNest (Nest _ d) = d
elideNest d          = d


-- ---------------------------------------------------------------------------
-- Selecting the best layout

best :: Int   -- Line length
     -> Int   -- Ribbon length
     -> RDocL m
     -> RDocL m -- No unions in here!
best w0 r p0
  = get w0 p0
  where
    get w _ | w == 0 && False = undefined
    get _ Empty               = Empty
    get _ NoDoc               = NoDoc
    get w (NilAbove p)        = nilAbove_ (get w p)
    get w (TextBeside s sl l p) = textBeside_ s sl l (get1 w sl p)
    get w (Nest k p)          = nest_ k (get (w - k) p)
    get w (p `Union` q)       = nicest w r (get w p) (get w q)
    get _ (Above {})          = error "best get Above"
    get _ (Beside {})         = error "best get Beside"

    get1 w _ _ | w == 0 && False  = undefined
    get1 _ _  Empty               = Empty
    get1 _ _  NoDoc               = NoDoc
    get1 w sl (NilAbove p)        = nilAbove_ (get (w - sl) p)
    get1 w sl (TextBeside t tl l p) = textBeside_ t tl l (get1 w (sl + tl) p)
    get1 w sl (Nest _ p)          = get1 w sl p
    get1 w sl (p `Union` q)       = nicest1 w r sl (get1 w sl p)
                                                   (get1 w sl q)
    get1 _ _  (Above {})          = error "best get1 Above"
    get1 _ _  (Beside {})         = error "best get1 Beside"

nicest :: Int -> Int -> DocL m -> DocL m -> DocL m
nicest !w !r p q = nicest1 w r 0 p q

nicest1 :: Int -> Int -> Int -> DocL m -> DocL m -> DocL m
nicest1 !w !r !sl p q | fits ((w `min` r) - sl) p = p
                      | otherwise                 = q

fits :: Int  -- Space available
     -> DocL m
     -> Bool -- True if *first line* of Doc fits in space available
fits n _ | n < 0           = False
fits _ NoDoc               = False
fits _ Empty               = True
fits _ (NilAbove _)        = True
fits n (TextBeside _ sl _ p) = fits (n - sl) p
fits _ (Above {})          = error "fits Above"
fits _ (Beside {})         = error "fits Beside"
fits _ (Union {})          = error "fits Union"
fits _ (Nest {})           = error "fits Nest"

-- | @first@ returns its first argument if it is non-empty, otherwise its second.
first :: DocL m -> DocL m -> DocL m
first p q | nonEmptySet p = p -- unused, because (get OneLineMode) is unused
          | otherwise     = q

nonEmptySet :: DocL m -> Bool
nonEmptySet NoDoc              = False
nonEmptySet (_ `Union` _)      = True
nonEmptySet Empty              = True
nonEmptySet (NilAbove _)       = True
nonEmptySet (TextBeside _ _ _ p) = nonEmptySet p
nonEmptySet (Nest _ p)         = nonEmptySet p
nonEmptySet (Above {})         = error "nonEmptySet Above"
nonEmptySet (Beside {})        = error "nonEmptySet Beside"

-- @oneLiner@ returns the one-line members of the given set of @GDoc@s.
oneLiner :: DocL m -> DocL m
oneLiner NoDoc               = NoDoc
oneLiner Empty               = Empty
oneLiner (NilAbove _)        = NoDoc
oneLiner (TextBeside s sl l p) = textBeside_ s sl l (oneLiner p)
oneLiner (Nest k p)          = nest_ k (oneLiner p)
oneLiner (p `Union` _)       = oneLiner p
oneLiner (Above {})          = error "oneLiner Above"
oneLiner (Beside {})         = error "oneLiner Beside"


-- ---------------------------------------------------------------------------
-- Rendering

-- | A rendering style.
data Style
  = Style { mode           :: Mode  -- ^ The rendering mode
          , lineLength     :: Int   -- ^ Length of line, in chars
          , ribbonsPerLine :: Float -- ^ Ratio of ribbon length to line length
          }

-- | The default style (@mode=PageMode, lineLength=100, ribbonsPerLine=1.5@).
style :: Style
style = Style { lineLength = 100, ribbonsPerLine = 1.5, mode = PageMode }

-- | Rendering mode.
data Mode = PageMode     -- ^ Normal
          | ZigZagMode   -- ^ With zig-zag cuts
          | LeftMode     -- ^ No indentation, infinitely long lines
          | OneLineMode  -- ^ All on one line

-- | Render the @Doc@ to a String using the default @Style@.
render :: Doc -> String
render = fst . renderWithLog

-- | Render the @Doc@ to a String using the default @Style@. Also retrieve the log of @here@s.
renderWithLog :: Monoid m => DocL m -> (String, m)
renderWithLog doc = fullRenderWithLog (mode style) (lineLength style) (ribbonsPerLine style)
                        txtPrinter "" doc

-- | Render the @Doc@ to a String using the given @Style@.
renderStyle :: Style -> Doc  -> String
renderStyle s = fst . renderStyleWithLog s

-- | Render the @Doc@ to a String using the given @Style@. Also retrieve the log of @here@s.
renderStyleWithLog :: Monoid m => Style -> DocL m -> (String, m)
renderStyleWithLog s doc = fullRenderWithLog (mode s) (lineLength s) (ribbonsPerLine s)
                    txtPrinter "" doc

-- | Default TextDetails printer
txtPrinter :: TextDetails -> String -> String
txtPrinter (Chr c)   s  = c:s
txtPrinter (Str s1)  s2 = s1 ++ s2
txtPrinter (PStr s1) s2 = s1 ++ s2

-- | The general re-ndering interface.
fullRender :: Mode                     -- ^ Rendering mode
           -> Int                      -- ^ Line length
           -> Float                    -- ^ Ribbons per line
           -> (TextDetails -> a -> a)  -- ^ What to do with text
           -> a                        -- ^ What to do at the end
           -> Doc                      -- ^ The document
           -> a                        -- ^ Result
fullRender m lineLen ribbons txt rest doc =
  fst $ fullRenderWithLog m lineLen ribbons txt rest doc

-- | The general re-ndering interface with logging.
fullRenderWithLog  :: Monoid m
                   => Mode                     -- ^ Rendering mode
                   -> Int                      -- ^ Line length
                   -> Float                    -- ^ Ribbons per line
                   -> (TextDetails -> a -> a)  -- ^ What to do with text
                   -> a                        -- ^ What to do at the end
                   -> DocL m                 -- ^ The document
                   -> (a, m)                   -- ^ Result
fullRenderWithLog OneLineMode _ _ txt end doc
  = easy_display space_text (\_ y -> y) txt end (reduceDoc doc)
fullRenderWithLog LeftMode    _ _ txt end doc
  = easy_display nl_text first txt end (reduceDoc doc)

fullRenderWithLog mode_ lineLen ribbons txt rest doc
  = display mode_ lineLen ribbonLen txt rest doc'
  where
    doc' = best bestLineLen ribbonLen (reduceDoc doc)

    bestLineLen, ribbonLen :: Int
    ribbonLen   = round (fromIntegral lineLen / ribbons)
    bestLineLen = case mode_ of
                      ZigZagMode -> maxBound
                      _          -> lineLen

easy_display :: Monoid m
             => TextDetails
             -> (DocL m -> DocL m -> DocL m)
             -> (TextDetails -> a -> a)
             -> a
             -> DocL m
             -> (a, m)
easy_display nl_space_text choose txt end doc
  = lay doc start mempty
  where
    lay NoDoc                       _ _ = error "easy_display: NoDoc"
    lay (Union p q)                 w m = lay (choose p q) w m
    lay (Nest _ p)                  w m = lay p w m
    lay Empty                       _ m = (end, m)
    lay (NilAbove p)                w m = let (s', m') = lay p (newline w) m in (nl_space_text `txt` s', m')
    lay (TextBeside s c l p)  w m = let (s', l') = lay p (advance w c) (log m l w) in (s `txt` s', l')
    lay (Above {})                  _ _ = error "easy_display Above"
    lay (Beside {})                 _ _ = error "easy_display Beside"

display :: Monoid m
        => Mode
        -> Int
        -> Int
        -> (TextDetails -> a -> a)
        -> a
        -> DocL m
        -> (a, m)
display mode_ !page_width !ribbon_width txt end doc
  = case page_width - ribbon_width of { gap_width ->
    case gap_width `quot` 2 of { shift ->
    let
        lay k _                     _ _ | k `seq` False = undefined
        lay k (Nest k1 p)           w m = lay (k + k1) p w m
        lay _ Empty                 _ m = (end, m)
        lay k (NilAbove p)          w m = let (s', m') = lay k p (newline w) m in (nl_text `txt` s', m')
        lay k (TextBeside s sl l p) w m
            = case mode_ of
                    ZigZagMode |  k >= gap_width
                               -> let (s', m') = lay1 (k - shift) s sl p (newline (newline w)) (log m l w) in
                                  (nl_text `txt` (
                                  Str (replicate shift '/') `txt` (
                                  nl_text `txt` s')), m')
                               |  k < 0
                               -> let (s', m') = lay1 (k + shift) s sl p (newline (newline w)) (log m l w) in
                                  (nl_text `txt` (
                                  Str (replicate shift '\\') `txt` (
                                  nl_text `txt` s')), m')

                    _ -> lay1 k s sl p w (log m l w)
        lay _ (Above {})            _ _ = error "display lay Above"
        lay _ (Beside {})           _ _ = error "display lay Beside"
        lay _ NoDoc                 _ _ = error "display lay NoDoc"
        lay _ (Union {})            _ _ = error "display lay Union"

        lay1 !k s !sl p w l    = let
                                  !r = k + sl
                                  (s', l') = lay2 r p (advance w (k + sl)) l
                             in (Str (indent k) `txt` (s `txt` s'), l')

        lay2 k _ _ _ | k `seq` False     = undefined
        lay2 k (NilAbove p)          w m = let (s', m') = lay k p (newline w) m in (nl_text `txt` s', m')
        lay2 k (TextBeside s sl l p) w m = let (s', m') = lay2 (k+sl) p (advance w sl) (log m l w) in (s `txt` s', m')
        lay2 k (Nest _ p)            w m = lay2 k p w m
        lay2 _ Empty                 _ m = (end, m)
        lay2 _ (Above {})            _ _ = error "display lay2 Above"
        lay2 _ (Beside {})           _ _ = error "display lay2 Beside"
        lay2 _ NoDoc                 _ _ = error "display lay2 NoDoc"
        lay2 _ (Union {})            _ _ = error "display lay2 Union"
    in
    lay 0 doc start mempty
    }}


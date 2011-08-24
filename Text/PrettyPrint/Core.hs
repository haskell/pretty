{-# LANGUAGE BangPatterns, CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Core
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David Terei <dave.terei@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A set of Pretty Printing Combinators, presenting the core of the
-- pretty Pretty Printing library. Here the underlying data type to
-- store string is abstracted by a type class to allow users to provide
-- their own high performance string data types.
--
-----------------------------------------------------------------------------
module Text.PrettyPrint.Core (

        -- * The document type
        GDoc(..), DocBase(..),

        -- * Constructing documents

        -- ** Converting values into documents
        char, text, sizedText, zeroWidthText,
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

        -- * Rendering documents
        Style(..), Mode(..),
        style, fullRender
    ) where

import Data.Monoid ( Monoid(mempty, mappend) )
import Data.String ( IsString(fromString) )

-- ---------------------------------------------------------------------------
-- The Doc calculus

-- The Doc combinators satisfy the following laws:

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
-- The GDoc data type

-- | The DocBase class
--
-- A DocBase data instance represents a fragment of text that will be
-- output at some point. We require at least support for Char and Strings
-- to be stored.
class DocBase a where
    chr :: Char -> a
    str :: String -> a

-- | The abstract type of documents.
-- A GDoc represents a *set* of layouts.  A GDoc with
-- no occurrences of Union or NoDoc represents just one layout.
data DocBase a => GDoc a
  = Empty                             -- empty
  | NilAbove (GDoc a)                 -- text "" $$ x
  | TextBeside !a {-# UNPACK #-} !Int (GDoc a)
                                      -- text s <> x
  | Nest {-# UNPACK #-} !Int (GDoc a) -- nest k x
  | Union (GDoc a) (GDoc a)           -- ul `union` ur
  | NoDoc                             -- The empty set of documents
  | Beside (GDoc a) Bool (GDoc a)     -- True <=> space between
  | Above  (GDoc a) Bool (GDoc a)     -- True <=> never overlap

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
type RDoc a = GDoc a

-- Combining @Doc@ values
instance DocBase a => Monoid (GDoc a) where
    mempty  = empty
    mappend = (<>)

instance DocBase a => IsString (GDoc a) where
    fromString = text

-- ---------------------------------------------------------------------------
-- Values and Predicates on GDocs and DocBase

-- | A document of height and width 1, containing a literal character.
char :: DocBase a => Char -> GDoc a
char c = textBeside_ (chr c) 1 Empty

-- | A document of height 1 containing a literal string.
-- 'text' satisfies the following laws:
--
-- * @'text' s '<>' 'text' t = 'text' (s'++'t)@
--
-- * @'text' \"\" '<>' x = x@, if @x@ non-empty
--
-- The side condition on the last law is necessary because @'text' \"\"@
-- has height 1, while 'empty' has no height.
text :: DocBase a => String -> GDoc a
text s = case length s of {sl -> textBeside_ (str s)  sl Empty}

-- | Some text with any width. (@text s = sizedText (length s) s@)
sizedText :: DocBase a => Int -> String -> GDoc a
sizedText l s = textBeside_ (str s) l Empty

-- | Some text, but without any width. Use for non-printing text
-- such as a HTML or Latex tags
zeroWidthText :: DocBase a => String -> GDoc a
zeroWidthText = sizedText 0

-- | The empty document, with no height and no width.
-- 'empty' is the identity for '<>', '<+>', '$$' and '$+$', and anywhere
-- in the argument list for 'sep', 'hcat', 'hsep', 'vcat', 'fcat' etc.
empty :: GDoc a
empty = Empty

-- | Returns 'True' if the document is empty
isEmpty :: GDoc a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- an old version inserted tabs being 8 columns apart in the output.
indent :: Int -> String
indent !n = replicate n ' '
{- TODO: GHC Optimised version
-- optimise long indentations using LitString chunks of 8 spaces
indent n r | n >=# _ILIT(8) = LStr (sLit "        ") (_ILIT(8)) `txt`
                              indent (n -# _ILIT(8)) r
           | otherwise      = Str (spaces n) `txt` r
-}

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


semi   :: DocBase a => GDoc a -- ^ A ';' character
comma  :: DocBase a => GDoc a -- ^ A ',' character
colon  :: DocBase a => GDoc a -- ^ A ':' character
space  :: DocBase a => GDoc a -- ^ A space character
equals :: DocBase a => GDoc a -- ^ A '=' character
lparen :: DocBase a => GDoc a -- ^ A '(' character
rparen :: DocBase a => GDoc a -- ^ A ')' character
lbrack :: DocBase a => GDoc a -- ^ A '[' character
rbrack :: DocBase a => GDoc a -- ^ A ']' character
lbrace :: DocBase a => GDoc a -- ^ A '{' character
rbrace :: DocBase a => GDoc a -- ^ A '}' character
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

space_text, nl_text :: DocBase a => a
space_text = chr ' '
nl_text    = chr '\n'

int      :: DocBase a => Int      -> GDoc a -- ^ @int n = text (show n)@
integer  :: DocBase a => Integer  -> GDoc a -- ^ @integer n = text (show n)@
float    :: DocBase a => Float    -> GDoc a -- ^ @float n = text (show n)@
double   :: DocBase a => Double   -> GDoc a -- ^ @double n = text (show n)@
rational :: DocBase a => Rational -> GDoc a -- ^ @rational n = text (show n)@
int      n = text (show n)
integer  n = text (show n)
float    n = text (show n)
double   n = text (show n)
rational n = text (show n)

parens       :: DocBase a => GDoc a -> GDoc a -- ^ Wrap document in @(...)@
brackets     :: DocBase a => GDoc a -> GDoc a -- ^ Wrap document in @[...]@
braces       :: DocBase a => GDoc a -> GDoc a -- ^ Wrap document in @{...}@
quotes       :: DocBase a => GDoc a -> GDoc a -- ^ Wrap document in @\'...\'@
doubleQuotes :: DocBase a => GDoc a -> GDoc a -- ^ Wrap document in @\"...\"@
quotes p       = char '\'' <> p <> char '\''
doubleQuotes p = char '"' <> p <> char '"'
parens p       = char '(' <> p <> char ')'
brackets p     = char '[' <> p <> char ']'
braces p       = char '{' <> p <> char '}'


-- ---------------------------------------------------------------------------
-- Structural operations on GDocs

-- | Perform some simplification of a built up @GDoc@.
reduceDoc :: DocBase a => GDoc a -> RDoc a
reduceDoc (Beside p g q) = beside p g (reduceDoc q)
reduceDoc (Above  p g q) = above  p g (reduceDoc q)
reduceDoc p              = p

-- | List version of '<>'.
hcat :: DocBase a => [GDoc a] -> GDoc a
hcat = reduceAB . foldr (beside_' False) empty

-- | List version of '<+>'.
hsep :: DocBase a => [GDoc a] -> GDoc a
hsep = reduceAB . foldr (beside_' True)  empty

-- | List version of '$$'.
vcat :: DocBase a => [GDoc a] -> GDoc a
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
nest :: DocBase a => Int -> GDoc a -> GDoc a
nest k p = mkNest k (reduceDoc p)

-- | @hang d1 n d2 = sep [d1, nest n d2]@
hang :: DocBase a => GDoc a -> Int -> GDoc a -> GDoc a
hang d1 n d2 = sep [d1, nest n d2]

-- | @punctuate p [d1, ... dn] = [d1 \<> p, d2 \<> p, ... dn-1 \<> p, dn]@
punctuate :: DocBase a => GDoc a -> [GDoc a] -> [GDoc a]
punctuate _ []     = []
punctuate p (x:xs) = go x xs
                   where go y []     = [y]
                         go y (z:zs) = (y <> p) : go z zs

-- mkNest checks for Nest's invariant that it doesn't have an Empty inside it
mkNest :: DocBase a => Int -> GDoc a -> GDoc a
mkNest k _ | k `seq` False = undefined
mkNest k (Nest k1 p)       = mkNest (k + k1) p
mkNest _ NoDoc             = NoDoc
mkNest _ Empty             = Empty
mkNest 0 p                 = p
mkNest k p                 = nest_ k p

-- mkUnion checks for an empty document
mkUnion :: DocBase a => GDoc a -> GDoc a -> GDoc a
mkUnion Empty _ = Empty
mkUnion p q     = p `union_` q

beside_' :: DocBase a => Bool -> GDoc a -> GDoc a -> GDoc a
beside_' _ p Empty = p
beside_' g p q     = Beside p g q

above_' :: DocBase a => Bool -> GDoc a -> GDoc a -> GDoc a
above_' _ p Empty = p
above_' g p q     = Above p g q

reduceAB :: DocBase a => GDoc a -> GDoc a
reduceAB (Above  Empty _ q) = q
reduceAB (Beside Empty _ q) = q
reduceAB doc                = doc

nilAbove_ :: DocBase a => RDoc a -> RDoc a
nilAbove_ p = NilAbove p

-- Arg of a TextBeside is always an RDoc
textBeside_ :: DocBase a => a -> Int -> RDoc a -> RDoc a
textBeside_ s sl p = TextBeside s sl p

nest_ :: DocBase a => Int -> RDoc a -> RDoc a
nest_ k p = Nest k p

union_ :: DocBase a => RDoc a -> RDoc a -> RDoc a
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
($$) :: DocBase a => GDoc a -> GDoc a -> GDoc a
p $$  q = above_ p False q

-- | Above, with no overlapping.
-- '$+$' is associative, with identity 'empty'.
($+$) :: DocBase a => GDoc a -> GDoc a -> GDoc a
p $+$ q = above_ p True q

above_ :: DocBase a => GDoc a -> Bool -> GDoc a -> GDoc a
above_ p _ Empty = p
above_ Empty _ q = q
above_ p g q     = Above p g q

above :: DocBase a => GDoc a -> Bool -> RDoc a -> RDoc a
above (Above p g1 q1)  g2 q2 = above p g1 (above q1 g2 q2)
above p@(Beside _ _ _) g  q  = aboveNest (reduceDoc p) g 0 (reduceDoc q)
above p g q                  = aboveNest p             g 0 (reduceDoc q)

aboveNest :: DocBase a => RDoc a -> Bool -> Int -> RDoc a -> RDoc a
-- Specfication: aboveNest p g k q = p $g$ (nest k q)

aboveNest _                   _ k _ | k `seq` False = undefined
aboveNest NoDoc               _ _ _ = NoDoc
aboveNest (p1 `Union` p2)     g k q = aboveNest p1 g k q `union_`
                                      aboveNest p2 g k q

aboveNest Empty               _ k q = mkNest k q
aboveNest (Nest k1 p)         g k q = nest_ k1 (aboveNest p g (k - k1) q)
                                  -- p can't be Empty, so no need for mkNest

aboveNest (NilAbove p)        g k q = nilAbove_ (aboveNest p g k q)
aboveNest (TextBeside s sl p) g k q = textBeside_ s sl rest
                                    where
                                      !k1  = k - sl
                                      rest = case p of
                                                Empty -> nilAboveNest g k1 q
                                                _     -> aboveNest  p g k1 q
aboveNest (Above {})          _ _ _ = error "aboveNest Above"
aboveNest (Beside {})         _ _ _ = error "aboveNest Beside"

nilAboveNest :: DocBase a => Bool -> Int -> RDoc a -> RDoc a
-- Specification: text s <> nilaboveNest g k q
--              = text s <> (text "" $g$ nest k q)

nilAboveNest _ k _           | k `seq` False = undefined
nilAboveNest _ _ Empty       = Empty
                               -- Here's why the "text s <>" is in the spec!
nilAboveNest g k (Nest k1 q) = nilAboveNest g (k + k1) q

nilAboveNest g k q           | not g && k > 0      -- No newline if no overlap
                             = textBeside_ (str (indent k)) k q
                             | otherwise           -- Put them really above
                             = nilAbove_ (mkNest k q)


-- ---------------------------------------------------------------------------
-- Horizontal composition @<>@

-- | Beside.
-- '<>' is associative, with identity 'empty'.
(<>) :: DocBase a => GDoc a -> GDoc a -> GDoc a
p <>  q = beside_ p False q

-- | Beside, separated by space, unless one of the arguments is 'empty'.
-- '<+>' is associative, with identity 'empty'.
(<+>) :: DocBase a => GDoc a -> GDoc a -> GDoc a
p <+> q = beside_ p True  q

beside_ :: DocBase a => GDoc a -> Bool -> GDoc a -> GDoc a
beside_ p _ Empty = p
beside_ Empty _ q = q
beside_ p g q     = Beside p g q

beside :: DocBase a => GDoc a -> Bool -> RDoc a -> RDoc a
-- Specification: beside g p q = p <g> q

beside NoDoc               _ _   = NoDoc
beside (p1 `Union` p2)     g q   = beside p1 g q `union_` beside p2 g q
beside Empty               _ q   = q
beside (Nest k p)          g q   = nest_ k $! beside p g q
beside p@(Beside p1 g1 q1) g2 q2
         | g1 == g2              = beside p1 g1 $! beside q1 g2 q2
         | otherwise             = beside (reduceDoc p) g2 q2
beside p@(Above _ _ _)     g q   = let !d = reduceDoc p in beside d g q
beside (NilAbove p)        g q   = nilAbove_ $! beside p g q
beside (TextBeside s sl p) g q   = textBeside_ s sl $! rest
                               where
                                  rest = case p of
                                           Empty -> nilBeside g q
                                           _     -> beside p g q

nilBeside :: DocBase a => Bool -> RDoc a -> RDoc a
-- Specification: text "" <> nilBeside g p
--              = text "" <g> p

nilBeside _ Empty         = Empty -- Hence the text "" in the spec
nilBeside g (Nest _ p)    = nilBeside g p
nilBeside g p | g         = textBeside_ space_text 1 p
              | otherwise = p


-- ---------------------------------------------------------------------------
-- Separate, @sep@

-- Specification: sep ps  = oneLiner (hsep ps)
--                         `union`
--                          vcat ps

-- | Either 'hsep' or 'vcat'.
sep  :: DocBase a => [GDoc a] -> GDoc a 
sep = sepX True   -- Separate with spaces

-- | Either 'hcat' or 'vcat'.
cat :: DocBase a => [GDoc a] -> GDoc a
cat = sepX False  -- Don't

sepX :: DocBase a => Bool -> [GDoc a] -> GDoc a
sepX _ []     = empty
sepX x (p:ps) = sep1 x (reduceDoc p) 0 ps


-- Specification: sep1 g k ys = sep (x : map (nest k) ys)
--                            = oneLiner (x <g> nest k (hsep ys))
--                              `union` x $$ nest k (vcat ys)

sep1 :: DocBase a => Bool -> RDoc a -> Int -> [GDoc a] -> RDoc a
sep1 _ _                   k _  | k `seq` False = undefined
sep1 _ NoDoc               _ _  = NoDoc
sep1 g (p `Union` q)       k ys = sep1 g p k ys `union_`
                                  aboveNest q False k (reduceDoc (vcat ys))

sep1 g Empty               k ys = mkNest k (sepX g ys)
sep1 g (Nest n p)          k ys = nest_ n (sep1 g p (k - n) ys)

sep1 _ (NilAbove p)        k ys = nilAbove_
                                  (aboveNest p False k (reduceDoc (vcat ys)))
sep1 g (TextBeside s sl p) k ys = textBeside_ s sl (sepNB g p (k - sl) ys)
sep1 _ (Above {})          _ _  = error "sep1 Above"
sep1 _ (Beside {})         _ _  = error "sep1 Beside"

sepNB :: DocBase a => Bool -> GDoc a -> Int -> [GDoc a] -> GDoc a
-- Specification: sepNB p k ys = sep1 (text "" <> p) k ys
-- Called when we have already found some text in the first item
-- We have to eat up nests

sepNB g (Nest _ p) k ys
  = sepNB g p k ys -- Never triggered, because of invariant (2)
sepNB g Empty k ys
  = oneLiner (nilBeside g (reduceDoc rest)) `mkUnion`
    nilAboveNest True k (reduceDoc (vcat ys))
  where
    rest | g         = hsep ys
         | otherwise = hcat ys
sepNB g p k ys
  = sep1 g p k ys


-- ---------------------------------------------------------------------------
-- @fill@

-- | \"Paragraph fill\" version of 'cat'.
fcat :: DocBase a => [GDoc a] -> GDoc a
fcat = fill False

-- | \"Paragraph fill\" version of 'sep'.
fsep :: DocBase a => [GDoc a] -> GDoc a
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

fill :: DocBase a => Bool -> [GDoc a] -> RDoc a
fill _ []     = empty
fill g (p:ps) = fill1 g (reduceDoc p) 0 ps

fill1 :: DocBase a => Bool -> RDoc a -> Int -> [GDoc a] -> GDoc a
fill1 _ _                   k _  | k `seq` False = undefined
fill1 _ NoDoc               _ _  = NoDoc
fill1 g (p `Union` q)       k ys = fill1 g p k ys `union_`
                                   aboveNest q False k (fill g ys)
fill1 g Empty               k ys = mkNest k (fill g ys)
fill1 g (Nest n p)          k ys = nest_ n (fill1 g p (k - n) ys)
fill1 g (NilAbove p)        k ys = nilAbove_ (aboveNest p False k (fill g ys))
fill1 g (TextBeside s sl p) k ys = textBeside_ s sl (fillNB g p (k - sl) ys)
fill1 _ (Above {})          _ _  = error "fill1 Above"
fill1 _ (Beside {})         _ _  = error "fill1 Beside"

fillNB :: DocBase a => Bool -> GDoc a -> Int -> [GDoc a] -> GDoc a
fillNB _ _           k _  | k `seq` False = undefined
fillNB g (Nest _ p)  k ys   = fillNB g p k ys
                              -- Never triggered, because of invariant (2)
fillNB _ Empty _ []         = Empty
fillNB g Empty k (Empty:ys) = fillNB g Empty k ys
fillNB g Empty k (y:ys)     = fillNBE g k y ys
fillNB g p k ys             = fill1 g p k ys

fillNBE :: DocBase a => Bool -> Int -> GDoc a -> [GDoc a] -> GDoc a
fillNBE g k y ys
  = nilBeside g (fill1 g ((elideNest . oneLiner . reduceDoc) y) k' ys)
    `mkUnion` nilAboveNest True k (fill g (y:ys))
  where k' = if g then k - 1 else k

elideNest :: DocBase a => GDoc a -> GDoc a
elideNest (Nest _ d) = d
elideNest d          = d


-- ---------------------------------------------------------------------------
-- Selecting the best layout

best :: DocBase a
     => Int     -- Line length
     -> Int     -- Ribbon length
     -> RDoc a
     -> RDoc a  -- No unions in here!
best w0 r p0
  = get w0 p0
  where
    get w _ | w == 0 && False = undefined
    get _ Empty               = Empty
    get _ NoDoc               = NoDoc
    get w (NilAbove p)        = nilAbove_ (get w p)
    get w (TextBeside s sl p) = textBeside_ s sl (get1 w sl p)
    get w (Nest k p)          = nest_ k (get (w - k) p)
    get w (p `Union` q)       = nicest w r (get w p) (get w q)
    get _ (Above {})          = error "best get Above"
    get _ (Beside {})         = error "best get Beside"

    get1 w _ _ | w == 0 && False  = undefined
    get1 _ _  Empty               = Empty
    get1 _ _  NoDoc               = NoDoc
    get1 w sl (NilAbove p)        = nilAbove_ (get (w - sl) p)
    get1 w sl (TextBeside t tl p) = textBeside_ t tl (get1 w (sl + tl) p)
    get1 w sl (Nest _ p)          = get1 w sl p
    get1 w sl (p `Union` q)       = nicest1 w r sl (get1 w sl p)
                                                   (get1 w sl q)
    get1 _ _  (Above {})          = error "best get1 Above"
    get1 _ _  (Beside {})         = error "best get1 Beside"

nicest :: DocBase a => Int -> Int -> GDoc a -> GDoc a -> GDoc a
nicest !w !r p q = nicest1 w r 0 p q

nicest1 :: DocBase a => Int -> Int -> Int -> GDoc a -> GDoc a -> GDoc a
nicest1 !w !r !sl p q | fits ((w `min` r) - sl) p = p
                      | otherwise                  = q

fits :: DocBase a
     => Int  -- Space available
     -> GDoc a
     -> Bool -- True if *first line* of Doc fits in space available
fits n _ | n < 0           = False
fits _ NoDoc               = False
fits _ Empty               = True
fits _ (NilAbove _)        = True
fits n (TextBeside _ sl p) = fits (n - sl) p
fits _ (Above {})          = error "fits Above"
fits _ (Beside {})         = error "fits Beside"
fits _ (Union {})          = error "fits Union"
fits _ (Nest {})           = error "fits Nest"

-- | @first@ returns its first argument if it is non-empty, otherwise its second.
first :: DocBase a => GDoc a -> GDoc a -> GDoc a
first p q | nonEmptySet p = p -- unused, because (get OneLineMode) is unused
          | otherwise     = q

nonEmptySet :: DocBase a => GDoc a -> Bool
nonEmptySet NoDoc              = False
nonEmptySet (_ `Union` _)      = True
nonEmptySet Empty              = True
nonEmptySet (NilAbove _)       = True
nonEmptySet (TextBeside _ _ p) = nonEmptySet p
nonEmptySet (Nest _ p)         = nonEmptySet p
nonEmptySet (Above {})         = error "nonEmptySet Above"
nonEmptySet (Beside {})        = error "nonEmptySet Beside"

-- @oneLiner@ returns the one-line members of the given set of @GDoc@s.
oneLiner :: DocBase a => GDoc a -> GDoc a
oneLiner NoDoc               = NoDoc
oneLiner Empty               = Empty
oneLiner (NilAbove _)        = NoDoc
oneLiner (TextBeside s sl p) = textBeside_ s sl (oneLiner p)
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

-- | The general rendering interface.
fullRender :: DocBase a
           => Mode                     -- ^ Rendering mode
           -> Int                      -- ^ Line length
           -> Float                    -- ^ Ribbons per line
           -> (a -> b -> b)  -- ^ What to do with text
           -> b                        -- ^ What to do at the end
           -> GDoc a                   -- ^ The document
           -> b                        -- ^ Result
fullRender OneLineMode _ _ txt end doc
  = easy_display space_text (\_ y -> y) txt end (reduceDoc doc)
fullRender LeftMode    _ _ txt end doc
  = easy_display nl_text first txt end (reduceDoc doc)

fullRender m lineLen ribbons txt rest doc
  = display m lineLen ribbonLen txt rest doc'
  where
    doc' = best bestLineLen ribbonLen (reduceDoc doc)

    bestLineLen, ribbonLen :: Int
    ribbonLen   = round (fromIntegral lineLen / ribbons)
    bestLineLen = case m of
                      ZigZagMode -> maxBound
                      _          -> lineLen

easy_display :: DocBase a
             => a
             -> (GDoc a -> GDoc a -> GDoc a)
             -> (a -> b -> b)
             -> b
             -> GDoc a
             -> b
easy_display nl_space_text choose txt end doc
  = lay doc
  where
    lay NoDoc              = error "easy_display: NoDoc"
    lay (Union p q)        = lay (choose p q)
    lay (Nest _ p)         = lay p
    lay Empty              = end
    lay (NilAbove p)       = nl_space_text `txt` lay p
    lay (TextBeside s _ p) = s `txt` lay p
    lay (Above {})         = error "easy_display Above"
    lay (Beside {})        = error "easy_display Beside"

display :: DocBase a => Mode -> Int -> Int -> (a -> b -> b) -> b -> GDoc a -> b
display m !page_width !ribbon_width txt end doc
  = case page_width - ribbon_width of { gap_width ->
    case gap_width `quot` 2 of { shift ->
    let
        lay k _            | k `seq` False = undefined
        lay k (Nest k1 p)  = lay (k + k1) p
        lay _ Empty        = end
        lay k (NilAbove p) = nl_text `txt` lay k p
        lay k (TextBeside s sl p)
            = case m of
                    ZigZagMode |  k >= gap_width
                               -> nl_text `txt` (
                                  str (replicate shift '/') `txt` (
                                  nl_text `txt`
                                  lay1 (k - shift) s sl p ))

                               |  k < 0
                               -> nl_text `txt` (
                                  str (replicate shift '\\') `txt` (
                                  nl_text `txt`
                                  lay1 (k + shift) s sl p ))

                    _ -> lay1 k s sl p
        lay _ (Above {})   = error "display lay Above"
        lay _ (Beside {})  = error "display lay Beside"
        lay _ NoDoc        = error "display lay NoDoc"
        lay _ (Union {})   = error "display lay Union"

        lay1 !k s !sl p    = let !r = k + sl
                             in str (indent k) `txt` (s `txt` lay2 r p)

        lay2 k _ | k `seq` False   = undefined
        lay2 k (NilAbove p)        = nl_text `txt` lay k p
        lay2 k (TextBeside s sl p) = s `txt` lay2 (k + sl) p
        lay2 k (Nest _ p)          = lay2 k p
        lay2 _ Empty               = end
        lay2 _ (Above {})          = error "display lay2 Above"
        lay2 _ (Beside {})         = error "display lay2 Beside"
        lay2 _ NoDoc               = error "display lay2 NoDoc"
        lay2 _ (Union {})          = error "display lay2 Union"
    in
    lay 0 doc
    }}


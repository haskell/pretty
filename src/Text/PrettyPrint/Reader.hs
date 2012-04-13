-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Reader
-- Copyright   :  (c) Warren Harris 2012
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Warren Harris <warrensomebody@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A wrapper around the John Hughes's and Simon Peyton Jones's Pretty
-- Printer combinators based on the ReaderT monad transformer, allowing
-- lookups to be performed during the pretty-printing process.
-----------------------------------------------------------------------------

module Text.PrettyPrint.Reader (

        -- * The document type
        PP, P.Doc, P.TextDetails(..),

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

        {-
        -- * Utility functions for documents
        first, reduceDoc,
        -- TODO: Should these be exported? Previously they weren't
        -- WH: These don't make sense because RDoc isn't exposed or otherwise used.
        -}

        -- * Rendering documents

        -- ** Default rendering
        render,

        -- ** Rendering with a particular style
        P.Style(..),
        P.style,
        renderStyle,
        P.Mode(..),

        -- ** General rendering
        fullRender,

        -- * State
        ask,
        lift

    ) where

import Control.Applicative hiding (empty)
import Control.Monad.Reader
--import Control.Monad.Trans.Class
import qualified Text.PrettyPrint.HughesPJ as P

--------------------------------------------------------------------------------
-- Operator fixity

infixl 6 <>
infixl 6 <+>
infixl 5 $$, $+$

--------------------------------------------------------------------------------

-- | PP is a ReaderT monad transformer that allows lookups to be
-- performed during the pretty-printing process. For instance, suppose
-- you have an abstract syntax with interned symbols:
--
-- > data Lang = ... | Symbol Id
--
-- and a lookup operation:
--
-- > symbolName :: Store -> Id -> IO String
--
-- A pretty-printer for this syntax can be written thus:
--
-- > pp :: Lang -> PP Store IO Doc
-- > pp (Symbol id) = do store <- ask
-- >                     name <- lift $ symbolName store id
-- >                     text name
-- > pp (...) = ...
type PP u m a = ReaderT u m a

-- ---------------------------------------------------------------------------
-- Values and Predicates on GDocs and TextDetails

-- | A document of height and width 1, containing a literal character.
char :: (Monad m, Applicative m) => Char -> PP u m P.Doc
char c = return $ P.char c

-- | A document of height 1 containing a literal string.
-- 'text' satisfies the following laws:
--
-- * @'text' s '<>' 'text' t = 'text' (s'++'t)@
--
-- * @'text' \"\" '<>' x = x@, if @x@ non-empty
--
-- The side condition on the last law is necessary because @'text' \"\"@
-- has height 1, while 'empty' has no height.
text :: (Monad m, Applicative m) => String -> PP u m P.Doc
text s = return $ P.text s

-- | Same as @text@. Used to be used for Bytestrings.
ptext :: (Monad m, Applicative m) => String -> PP u m P.Doc
ptext s = return $ P.ptext s

-- | Some text with any width. (@text s = sizedText (length s) s@)
sizedText :: (Monad m, Applicative m) => Int -> String -> PP u m P.Doc
sizedText l s = return $ P.sizedText l s

-- | Some text, but without any width. Use for non-printing text
-- such as a HTML or Latex tags
zeroWidthText :: (Monad m, Applicative m) => String -> PP u m P.Doc
zeroWidthText s = return $ P.zeroWidthText s

-- | The empty document, with no height and no width.
-- 'empty' is the identity for '<>', '<+>', '$$' and '$+$', and anywhere
-- in the argument list for 'sep', 'hcat', 'hsep', 'vcat', 'fcat' etc.
empty :: (Monad m, Applicative m) => PP u m P.Doc
empty = return P.empty

-- | Returns 'True' if the document is empty
isEmpty :: (Monad m, Applicative m) => PP u m P.Doc -> PP u m Bool
isEmpty doc = P.isEmpty <$> doc

-- | A ';' character
semi   :: (Monad m, Applicative m) => PP u m P.Doc
semi = return P.semi

-- | A ',' character
comma  :: (Monad m, Applicative m) => PP u m P.Doc
comma = return P.comma

-- | A ':' character
colon  :: (Monad m, Applicative m) => PP u m P.Doc
colon = return P.colon

-- | A space character
space  :: (Monad m, Applicative m) => PP u m P.Doc
space = return P.space

-- | A '=' character
equals :: (Monad m, Applicative m) => PP u m P.Doc
equals = return P.equals

-- | A '(' character
lparen :: (Monad m, Applicative m) => PP u m P.Doc
lparen = return P.lparen

-- | A ')' character
rparen :: (Monad m, Applicative m) => PP u m P.Doc
rparen = return P.rparen

-- | A '[' character
lbrack :: (Monad m, Applicative m) => PP u m P.Doc
lbrack = return P.lbrack

-- | A ']' character
rbrack :: (Monad m, Applicative m) => PP u m P.Doc
rbrack = return P.rbrack

-- | A '{' character
lbrace :: (Monad m, Applicative m) => PP u m P.Doc
lbrace = return P.lbrace

-- | A '}' character
rbrace :: (Monad m, Applicative m) => PP u m P.Doc
rbrace = return P.rbrace

-- | @int n = text (show n)@
int      :: (Monad m, Applicative m) => Int      -> PP u m P.Doc
int      = return . P.int

-- | @integer n = text (show n)@
integer  :: (Monad m, Applicative m) => Integer  -> PP u m P.Doc
integer  = return . P.integer

-- | @float n = text (show n)@
float    :: (Monad m, Applicative m) => Float    -> PP u m P.Doc
float    = return . P.float

-- | @double n = text (show n)@
double   :: (Monad m, Applicative m) => Double   -> PP u m P.Doc
double   = return . P.double

-- | @rational n = text (show n)@
rational :: (Monad m, Applicative m) => Rational -> PP u m P.Doc
rational = return . P.rational

-- | Wrap document in @(...)@
parens       :: (Monad m, Applicative m) => PP u m P.Doc -> PP u m P.Doc
parens p     = P.parens <$> p

-- | Wrap document in @[...]@
brackets     :: (Monad m, Applicative m) => PP u m P.Doc -> PP u m P.Doc
brackets p   = P.brackets <$> p

-- | Wrap document in @{...}@
braces       :: (Monad m, Applicative m) => PP u m P.Doc -> PP u m P.Doc
braces p     = P.braces <$> p

-- | Wrap document in @\'...\'@
quotes       :: (Monad m, Applicative m) => PP u m P.Doc -> PP u m P.Doc
quotes p     = P.quotes <$> p

-- | Wrap document in @\"...\"@
doubleQuotes :: (Monad m, Applicative m) => PP u m P.Doc -> PP u m P.Doc
doubleQuotes p = P.doubleQuotes <$> p

-- ---------------------------------------------------------------------------
-- Structural operations on GDocs

{-
-- | Perform some simplification of a built up @GDoc@.
reduceDoc :: (Monad m, Applicative m) => PP u m P.Doc -> PP u m P.RDoc
reduceDoc p = p >>= return . P.reduceDoc
-}

-- | List version of '<>'.
hcat :: (Monad m, Applicative m) => PP u m [P.Doc] -> PP u m P.Doc
hcat l = P.hcat <$> l

-- | List version of '<+>'.
hsep :: (Monad m, Applicative m) => PP u m [P.Doc] -> PP u m P.Doc
hsep l = P.hsep <$> l

-- | List version of '$$'.
vcat :: (Monad m, Applicative m) => PP u m [P.Doc] -> PP u m P.Doc
vcat l = P.vcat <$> l

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
nest :: (Monad m, Applicative m) => Int -> PP u m P.Doc -> PP u m P.Doc
nest k p = P.nest k <$> p

-- | @hang d1 n d2 = sep [d1, nest n d2]@
hang :: (Monad m, Applicative m) => PP u m P.Doc -> Int -> PP u m P.Doc -> PP u m P.Doc
--hang d1 n d2 = do d1' <- d1; d2' <- d2; return $ P.hang d1' n d2'
hang d1 n d2 = flip P.hang n <$> d1 <*> d2

-- | @punctuate p [d1, ... dn] = [d1 \<> p, d2 \<> p, ... dn-1 \<> p, dn]@
punctuate :: (Monad m, Applicative m) => PP u m P.Doc -> PP u m [P.Doc] -> PP u m [P.Doc]
punctuate p l = P.punctuate <$> p <*> l

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
($$) :: (Monad m, Applicative m) => PP u m P.Doc -> PP u m P.Doc -> PP u m P.Doc
p $$ q = (P.$$) <$> p <*> q

-- | Above, with no overlapping.
-- '$+$' is associative, with identity 'empty'.
($+$) :: (Monad m, Applicative m) => PP u m P.Doc -> PP u m P.Doc -> PP u m P.Doc
p $+$ q = (P.$+$) <$> p <*> q

-- ---------------------------------------------------------------------------
-- Horizontal composition @<>@

-- We intentionally avoid Data.Monoid.(<>) here due to interactions of
-- Data.Monoid.(<>) and (<+>).  See
-- http://www.haskell.org/pipermail/libraries/2011-November/017066.html

-- | Beside.
-- '<>' is associative, with identity 'empty'.
(<>) :: (Monad m, Applicative m) => PP u m P.Doc -> PP u m P.Doc -> PP u m P.Doc
p <> q = (P.<>) <$> p <*> q

-- | Beside, separated by space, unless one of the arguments is 'empty'.
-- '<+>' is associative, with identity 'empty'.
(<+>) :: (Monad m, Applicative m) => PP u m P.Doc -> PP u m P.Doc -> PP u m P.Doc
p <+> q = (P.<+>) <$> p <*> q

-- ---------------------------------------------------------------------------
-- Separate, @sep@

-- Specification: sep ps  = oneLiner (hsep ps)
--                         `union`
--                          vcat ps

-- | Either 'hsep' or 'vcat'.
sep :: (Monad m, Applicative m) => PP u m [P.Doc] -> PP u m P.Doc
sep l = P.sep <$> l

-- | Either 'hcat' or 'vcat'.
cat :: (Monad m, Applicative m) => PP u m [P.Doc] -> PP u m P.Doc
cat l = P.cat <$> l

-- ---------------------------------------------------------------------------
-- @fill@

-- | \"Paragraph fill\" version of 'cat'.
fcat :: (Monad m, Applicative m) => PP u m [P.Doc] -> PP u m P.Doc
fcat l = P.fcat <$> l

-- | \"Paragraph fill\" version of 'sep'.
fsep :: (Monad m, Applicative m) => PP u m [P.Doc] -> PP u m P.Doc
fsep l = P.fsep <$> l

-- ---------------------------------------------------------------------------
-- Selecting the best layout
{-
-- | @first@ returns its first argument if it is non-empty, otherwise its second.
first :: (Monad m, Applicative m) => PP u m P.Doc -> PP u m P.Doc -> PP u m P.Doc
first p q = do p' <- p; q' <- q; return $ P.first p' q'
-}
-- ---------------------------------------------------------------------------
-- Rendering

-- | Render the @Doc@ to a String using the default @Style@.
render :: (Monad m, Applicative m) => u -> PP u m P.Doc -> m String
render u doc = runReaderT doc u >>= return . P.render

-- | Render the @Doc@ to a String using the given @Style@.
renderStyle :: (Monad m, Applicative m) => u -> P.Style -> PP u m P.Doc -> m String
renderStyle u s doc = runReaderT doc u >>= return . P.renderStyle s

-- | The general rendering interface.
fullRender :: (Monad m, Applicative m) =>
              u                        -- ^ User-defined state
           -> P.Mode                   -- ^ Rendering mode
           -> Int                      -- ^ Line length
           -> Float                    -- ^ Ribbons per line
           -> (P.TextDetails -> a -> a)  -- ^ What to do with text
           -> a                        -- ^ What to do at the end
           -> PP u m P.Doc             -- ^ The document
           -> m a                      -- ^ Result
fullRender u m lineLen ribbons txt rest doc =
  runReaderT doc u >>= return . P.fullRender m lineLen ribbons txt rest

--------------------------------------------------------------------------------

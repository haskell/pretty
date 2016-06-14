#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Annotated.HughesPJClass
-- Copyright   :  (c) Trevor Elliott <revor@galois.com> 2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David Terei <code@davidterei.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Pretty printing class, simlar to 'Show' but nicer looking.
--
-- Note that the precedence level is a 'Rational' so there is an unlimited
-- number of levels. This module re-exports
-- 'Text.PrettyPrint.Annotated.HughesPJ'.
--
-----------------------------------------------------------------------------

module Text.PrettyPrint.Annotated.HughesPJClass (
    -- * Pretty typeclass
    Pretty(..),

    PrettyLevel(..), prettyNormal,
    prettyShow, prettyParen,

    -- re-export HughesPJ
    module Text.PrettyPrint.Annotated.HughesPJ
  ) where

import Text.PrettyPrint.Annotated.HughesPJ
import Data.String

-- | Level of detail in the pretty printed output. Level 0 is the least
-- detail.
newtype PrettyLevel = PrettyLevel Int
  deriving (Eq, Ord, Show)

-- | The "normal" (Level 0) of detail.
prettyNormal :: PrettyLevel
prettyNormal = PrettyLevel 0

-- | Pretty printing class. The precedence level is used in a similar way as in
-- the 'Show' class. Minimal complete definition is either 'pPrintPrec' or
-- 'pPrint'.
class Chars string => Pretty string a where
  pPrintPrec :: PrettyLevel -> Rational -> a -> Doc string ann
  pPrintPrec _ _ = pPrint

  pPrint :: a -> Doc string ann
  pPrint = pPrintPrec prettyNormal 0

  pPrintList :: PrettyLevel -> [a] -> Doc string ann
  pPrintList l = brackets . fsep . punctuate comma . fmap (pPrintPrec l 0)

#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL pPrintPrec | pPrint #-}
#endif

-- | Pretty print a value with the 'prettyNormal' level.
prettyShow :: (Pretty string a) => a -> string
prettyShow = render . pPrint

pPrint0 :: (Pretty string a) => PrettyLevel -> a -> Doc string ann
pPrint0 l = pPrintPrec l 0

appPrec :: Rational
appPrec = 10

-- | Parenthesize an value if the boolean is true.
{-# DEPRECATED prettyParen "Please use 'maybeParens' instead" #-}
prettyParen :: Chars string => Bool -> Doc string ann -> Doc string ann
prettyParen = maybeParens

-- Various Pretty instances
instance Chars string => Pretty string Int where pPrint = int

instance Chars string => Pretty string Integer where pPrint = integer

instance Chars string => Pretty string Float where pPrint = float

instance Chars string => Pretty string Double where pPrint = double

instance Chars string => Pretty string () where pPrint _ = text "()"

instance Chars string => Pretty string Bool where pPrint = text . fromString . show

instance Chars string => Pretty string Ordering where pPrint = text . fromString . show

instance Chars string => Pretty string Char where
  pPrint = char
  pPrintList _ = text . fromString . show

instance (Chars string, Pretty string a) => Pretty string (Maybe a) where
  pPrintPrec _ _ Nothing = text "Nothing"
  pPrintPrec l p (Just x) =
    prettyParen (p > appPrec) $ text "Just" <+> pPrintPrec l (appPrec+1) x

instance (Chars string, Pretty string a, Pretty string b) => Pretty string (Either a b) where
  pPrintPrec l p (Left x) =
    prettyParen (p > appPrec) $ text "Left" <+> pPrintPrec l (appPrec+1) x
  pPrintPrec l p (Right x) =
    prettyParen (p > appPrec) $ text "Right" <+> pPrintPrec l (appPrec+1) x

instance (Chars string, Pretty string a) => Pretty string [a] where
  pPrintPrec l _ = pPrintList l

instance (Chars string, Pretty string a, Pretty string b) => Pretty string (a, b) where
  pPrintPrec l _ (a, b) =
    parens $ fsep $ punctuate comma [pPrint0 l a, pPrint0 l b]

instance (Chars string, Pretty string a, Pretty string b, Pretty string c) => Pretty string (a, b, c) where
  pPrintPrec l _ (a, b, c) =
    parens $ fsep $ punctuate comma [pPrint0 l a, pPrint0 l b, pPrint0 l c]

instance (Chars string, Pretty string a, Pretty string b, Pretty string c, Pretty string d) => Pretty string (a, b, c, d) where
  pPrintPrec l _ (a, b, c, d) =
    parens $ fsep $ punctuate comma
      [pPrint0 l a, pPrint0 l b, pPrint0 l c, pPrint0 l d]

instance (Pretty string a, Pretty string b, Pretty string c, Pretty string d, Pretty string e) => Pretty string (a, b, c, d, e) where
  pPrintPrec l _ (a, b, c, d, e) =
    parens $ fsep $ punctuate comma
      [pPrint0 l a, pPrint0 l b, pPrint0 l c, pPrint0 l d, pPrint0 l e]

instance (Pretty string a, Pretty string b, Pretty string c, Pretty string d, Pretty string e, Pretty string f) => Pretty string (a, b, c, d, e, f) where
  pPrintPrec l _ (a, b, c, d, e, f) =
    parens $ fsep $ punctuate comma
      [pPrint0 l a, pPrint0 l b, pPrint0 l c,
        pPrint0 l d, pPrint0 l e, pPrint0 l f]

instance (Pretty string a, Pretty string b, Pretty string c, Pretty string d, Pretty string e, Pretty string f, Pretty string g) =>
         Pretty string (a, b, c, d, e, f, g) where
  pPrintPrec l _ (a, b, c, d, e, f, g) =
    parens $ fsep $ punctuate comma
      [pPrint0 l a, pPrint0 l b, pPrint0 l c,
        pPrint0 l d, pPrint0 l e, pPrint0 l f, pPrint0 l g]

instance (Pretty string a, Pretty string b, Pretty string c, Pretty string d, Pretty string e, Pretty string f, Pretty string g, Pretty string h) =>
         Pretty string (a, b, c, d, e, f, g, h) where
  pPrintPrec l _ (a, b, c, d, e, f, g, h) =
    parens $ fsep $ punctuate comma
      [pPrint0 l a, pPrint0 l b, pPrint0 l c,
        pPrint0 l d, pPrint0 l e, pPrint0 l f, pPrint0 l g, pPrint0 l h]


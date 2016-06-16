#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.HughesPJClass
-- Copyright   :  (c) Lennart Augustsson 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David Terei <code@davidterei.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Pretty printing class, simlar to 'Show' but nicer looking. 
--
-- Note that the precedence level is a 'Rational' so there is an unlimited
-- number of levels. This module re-exports 'Text.PrettyPrint.HughesPJ'.
--
-----------------------------------------------------------------------------

module Text.PrettyPrint.Annotated.HughesPJClass.TextLazy (
    -- * Pretty typeclass
    Pretty(..),

    PrettyLevel(..), prettyNormal,
    prettyShow, prettyParen,

    -- re-export HughesPJ
    module Text.PrettyPrint.Annotated.ListLike
  ) where

import Text.PrettyPrint.Annotated.TextLazy
import Text.PrettyPrint.Annotated.ListLike (maybeParens)
import Text.PrettyPrint.Annotated.HughesPJClass.ListLike (PrettyLevel(..), prettyNormal)
import qualified Text.PrettyPrint.Annotated.HughesPJClass.ListLike as ListLike (Pretty(..))
import Data.Text.Lazy (Text)

-- | Pretty printing class. The precedence level is used in a similar way as in
-- the 'Show' class. Minimal complete definition is either 'pPrintPrec' or
-- 'pPrint'.
class Pretty a where
  pPrintPrec :: PrettyLevel -> Rational -> a -> Doc ann
  pPrintPrec _ _ = pPrint

  pPrint :: a -> Doc ann
  pPrint = pPrintPrec prettyNormal 0

  pPrintList :: PrettyLevel -> [a] -> Doc ann
  pPrintList l = brackets . fsep . punctuate comma . fmap (pPrintPrec l 0)

#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL pPrintPrec | pPrint #-}
#endif

-- | Pretty print a value with the 'prettyNormal' level.
prettyShow :: (Pretty a) => a -> Text
prettyShow = render . pPrint

-- | Parenthesize an value if the boolean is true.
{-# DEPRECATED prettyParen "Please use 'maybeParens' instead" #-}
prettyParen :: Bool -> Doc ann -> Doc ann
prettyParen = maybeParens

newtype P a = P {unP :: a}

instance ListLike.Pretty Text a => Pretty (P a) where
    pPrintPrec prec l (P x) = ListLike.pPrintPrec prec l x
    pPrint (P x) = ListLike.pPrint x
    pPrintList l xs = ListLike.pPrintList l (map unP xs)

-- Various Pretty instances
instance Pretty Int where pPrint = pPrint . P
instance Pretty Integer where pPrint = pPrint . P
instance Pretty Float where pPrint = pPrint . P
instance Pretty Double where pPrint = pPrint . P
instance Pretty () where pPrint = pPrint . P
instance Pretty Bool where pPrint = pPrint . P
instance Pretty Ordering where pPrint = pPrint . P
instance Pretty Char where pPrint = pPrint . P
instance (ListLike.Pretty Text a) => Pretty (Maybe a) where pPrint = pPrint . P
instance (ListLike.Pretty Text a, ListLike.Pretty Text b) => Pretty (Either a b) where pPrint = pPrint . P
instance (ListLike.Pretty Text a) => Pretty [a] where pPrint = pPrint . P
instance (ListLike.Pretty Text a, ListLike.Pretty Text b) => Pretty (a, b) where pPrint = pPrint . P
instance (ListLike.Pretty Text a, ListLike.Pretty Text b, ListLike.Pretty Text c) => Pretty (a, b, c) where pPrint = pPrint . P
instance (ListLike.Pretty Text a, ListLike.Pretty Text b, ListLike.Pretty Text c, ListLike.Pretty Text d) => Pretty (a, b, c, d) where pPrint = pPrint . P
instance (ListLike.Pretty Text a, ListLike.Pretty Text b, ListLike.Pretty Text c, ListLike.Pretty Text d, ListLike.Pretty Text e) => Pretty (a, b, c, d, e) where pPrint = pPrint . P
instance (ListLike.Pretty Text a, ListLike.Pretty Text b, ListLike.Pretty Text c, ListLike.Pretty Text d, ListLike.Pretty Text e, ListLike.Pretty Text f) => Pretty (a, b, c, d, e, f) where pPrint = pPrint . P
instance (ListLike.Pretty Text a, ListLike.Pretty Text b, ListLike.Pretty Text c, ListLike.Pretty Text d, ListLike.Pretty Text e, ListLike.Pretty Text f, ListLike.Pretty Text g) =>
         Pretty (a, b, c, d, e, f, g) where pPrint = pPrint . P
instance (ListLike.Pretty Text a, ListLike.Pretty Text b, ListLike.Pretty Text c, ListLike.Pretty Text d, ListLike.Pretty Text e, ListLike.Pretty Text f, ListLike.Pretty Text g, ListLike.Pretty Text h) =>
         Pretty (a, b, c, d, e, f, g, h) where pPrint = pPrint . P

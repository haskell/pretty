-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  David Terei <dave.terei@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- The default interface to the pretty-printing library. Provides a collection
-- of pretty printer combinators.
--
-- This should be used as opposed to the "Text.PrettyPrint.HughesPJ" module that
-- contains the actual implementation that this module simply re-exports.
--
-----------------------------------------------------------------------------

module Text.PrettyPrint ( 
        module Text.PrettyPrint.HughesPJ
    ) where

import Text.PrettyPrint.HughesPJ


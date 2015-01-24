#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Annotated
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
-- This module should be used as opposed to the "Text.PrettyPrint.HughesPJ"
-- module. Both are equivalent though as this module simply re-exports the
-- other.
--
-----------------------------------------------------------------------------

module Text.PrettyPrintAnnotated ( 

        -- * The document type
        Doc,

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

        -- ** Annotating documents
        annotate,

        -- * Predicates on documents
        isEmpty,

        -- * Rendering documents

        -- ** Default rendering
        render,

        -- ** Annotation rendering
        renderSpans, Span(..),

        -- ** Rendering with a particular style
        Style(..),
        style,
        renderStyle,

        -- ** General rendering
        fullRender,
        fullRenderAnn,
        Mode(..), TextDetails(..)

    ) where

import Text.PrettyPrint.Annotated.HughesPJ


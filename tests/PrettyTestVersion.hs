{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DeriveGeneric #-}
#endif

#define TESTING

-- | Here we use some CPP hackery to get a whitebox
-- version of HughesPJ for testing purposes.
module PrettyTestVersion where

#include "ListLike.hs"


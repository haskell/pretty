{-# LANGUAGE CPP #-}

#define TESTING

-- | Here we use some CPP hackery to get a whitebox
-- version of HughesPJ for testing purposes.
module PrettyTestVersion where

#include "HughesPJ.hs"


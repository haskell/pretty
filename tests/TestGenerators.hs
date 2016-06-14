{-# LANGUAGE UndecidableInstances #-}

-- | Test generators.
--
module TestGenerators (
        emptyDocGen,
        emptyDocListGen
    ) where

import PrettyTestVersion
import TestStructures

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Monad
import Data.String (fromString)

import Test.QuickCheck

instance Chars string => Arbitrary (CDoc string) where
   arbitrary = sized arbDoc
    where
      -- TODO: finetune frequencies
      arbDoc k | k <= 1 = frequency [
               (1,return CEmpty)
             , (2,return (CText . fromString . unText) `ap` arbitrary)
             ]
      arbDoc n = frequency [
             (1, return CList `ap` arbitrary `ap`  (liftM unDocList $ resize (pred n) arbitrary))
            ,(1, binaryComb n CBeside)
            ,(1, binaryComb n CAbove)
            ,(1, choose (0,10) >>= \k -> return (CNest k) `ap` (resize (pred n) arbitrary)) 
            ]
      binaryComb n f = 
        split2 (n-1) >>= \(n1,n2) ->
        return f `ap` arbitrary `ap` (resize n1 arbitrary) `ap` (resize n2 arbitrary)
      split2 n = flip liftM ( choose (0,n) ) $ \sz -> (sz, n - sz)

instance Chars string => CoArbitrary (CDoc string) where
   coarbitrary CEmpty = variant 0
   coarbitrary (CText t) = variant 1 . coarbitrary (PrettyTestVersion.length t)
   coarbitrary (CList f list) = variant 2 . coarbitrary f . coarbitrary list
   coarbitrary (CBeside b d1 d2) = variant 3 . coarbitrary b . coarbitrary d1 . coarbitrary d2
   coarbitrary (CAbove b d1 d2) = variant 4 . coarbitrary b . coarbitrary d1 . coarbitrary d2
   coarbitrary (CNest k d) = variant 5 . coarbitrary k . coarbitrary d
   
instance Arbitrary CList where
    arbitrary = oneof $ fmap return [ CCat, CSep, CFCat, CFSep ]

instance CoArbitrary CList where
    coarbitrary cl = variant (case cl of CCat -> 0; CSep -> 1; CFCat -> 2; CFSep -> 3)

-- we assume that the list itself has no size, so that 
-- sizeof (a $$ b) = sizeof (sep [a,b]) = sizeof(a) + sizeof(b)+1
instance Chars string => Arbitrary (CDocList string) where
    arbitrary = liftM CDocList $ sized $ \n -> arbDocList n where
        arbDocList 0 = return []
        arbDocList n = do
          listSz <- choose (1,n)
          let elems = take listSz $ repeat (n `div` listSz) -- approximative
          mapM (\sz -> resize sz arbitrary) elems

instance Chars string => CoArbitrary (CDocList string) where
    coarbitrary (CDocList ds) = coarbitrary ds

instance Chars string => Arbitrary (Text string) where
    arbitrary = liftM Text $ sized $ \n -> (fromString <$> mapM (const arbChar) [1..n])
        where arbChar = oneof (fmap return ['a'..'c'])

instance Chars string => CoArbitrary (Text string) where
    coarbitrary (Text str) = coarbitrary (PrettyTestVersion.length str)

emptyDocGen :: Gen (CDoc string)
emptyDocGen = return CEmpty

emptyDocListGen :: Gen (CDocList string)
emptyDocListGen = do
    ls <- listOf emptyDocGen
    return $ CDocList ls


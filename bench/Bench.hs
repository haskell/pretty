{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Criterion.Main
import qualified Data.ByteString.Char8 as B
import Data.String
import Data.List
import Text.PrettyPrint.Annotated.HughesPJ (txtPrinter, RuneSequence(..))
import Text.PrettyPrint.HughesPJ
import Data.Proxy

instance RuneSequence B.ByteString where
  len    = B.length
  unpack = B.unpack

--------------------------------------------------------------------------------
f_left :: Int -> Doc
f_left n = foldl' (<>) empty (map (text . show) [10001..10000+n])

--------------------------------------------------------------------------------
f_right :: Int -> Doc
f_right n = foldr (<>) empty (map (text . show) [10001..10000+n])

--------------------------------------------------------------------------------
stuff :: (IsString r, RuneSequence r) => Proxy r -> r -> r -> Double -> Rational -> Int -> Int -> Int -> Doc
stuff proxy s1 s2 d1 r1 i1 i2 i3 =
    let a = nest i1 $ text s1
        b = double d1
        c = rational r1
        d = replicate i1 (text s2 <> b <> c <+> a)
        e = cat d $+$ cat d $$ (c <> b <+> a)
        f = parens e <> brackets c <> hcat d
        g = lparen <> f <> rparen
        h = text $ (append proxy s2 s1)
        i = map rational ([1..(toRational i2)]::[Rational])
        j = punctuate comma i
        k = nest i3 h <> (nest (i1 + i3) $ sep i) $+$ g <> cat j
        l = cat $ punctuate (comma <> b <> comma) $ replicate i3 k
    in l

--------------------------------------------------------------------------------
append :: RuneSequence r => Proxy r -> r -> r -> r
append Proxy = mappend

--------------------------------------------------------------------------------
doc1 :: (IsString r, RuneSequence r) => Proxy r -> Doc
doc1 p = stuff p "Adsas ads" "dassdab weeaa xxxxx" 123.231321 ((-1)/5) 30 300 40

----------------------------------------------------------------------------------
doc2 :: (IsString r, RuneSequence r) => Proxy r -> Doc
doc2 p = stuff p "aDSAS ADS asdasdsa sdsda xx" "SDAB WEEAA" 1333.212 ((-4)/5) 31 301 60

----------------------------------------------------------------------------------
doc3 :: (IsString r, RuneSequence r) => Proxy r -> Doc
doc3 p = stuff p "ADsAs --____ aDS" "DasSdAB weEAA" 2533.21299 ((-4)/999) 39 399 120

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ [
  bgroup "<> associativity" [ bench "left"     $ nf (length . render . f_left)  10000
                            , bench "right"    $ nf (length . render . f_right) 10000
                            , bench "left20k"  $ nf (length . render . f_left)  20000
                            , bench "right20k" $ nf (length . render . f_right) 20000
                            , bench "left30k"  $ nf (length . render . f_left)  30000
                            , bench "right30k" $ nf (length . render . f_right) 30000
                            ]

  , bgroup "render" [ bench "doc1/string"     $ nf render (doc1 strPxy)
                    , bench "doc1/bytestring" $ nf render (doc1 bsPxy)
                    , bench "doc2/string"     $ nf render (doc2 strPxy)
                    , bench "doc2/bytestring" $ nf render (doc2 bsPxy)
                    , bench "doc3/string"     $ nf render (doc3 strPxy)
                    , bench "doc2/bytestring" $ nf render (doc3 bsPxy)
                    ]

  , bgroup "fullRender" [ bench "PageMode 1000/string"     $ nf (fullRender PageMode 1000 4 txtPrinter "") (doc2 strPxy)
                        , bench "PageMode 1000/bytestring" $ nf (fullRender PageMode 1000 4 txtPrinter "") (doc2 bsPxy)
                        , bench "PageMode 100/string"      $ nf (fullRender PageMode 100 1.5 txtPrinter "") (doc2 strPxy)
                        , bench "PageMode 100/bytestring"  $ nf (fullRender PageMode 100 1.5 txtPrinter "") (doc2 bsPxy)
                        , bench "ZigZagMode/string"        $ nf (fullRender ZigZagMode 1000 4 txtPrinter "") (doc2 strPxy)
                        , bench "ZigZagMode/bytestring"    $ nf (fullRender ZigZagMode 1000 4 txtPrinter "") (doc2 bsPxy)
                        , bench "LeftMode/string"          $ nf (fullRender LeftMode 1000 4 txtPrinter "") (doc2 strPxy)
                        , bench "LeftMode/bytestring"      $ nf (fullRender LeftMode 1000 4 txtPrinter "") (doc2 bsPxy)
                        , bench "OneLineMode/string"       $ nf (fullRender OneLineMode 1000 4 txtPrinter "") (doc3 strPxy)
                        , bench "OneLineMode/bytestring"   $ nf (fullRender OneLineMode 1000 4 txtPrinter "") (doc3 bsPxy)
                        ]
  ]
  where
    strPxy :: Proxy String
    strPxy = Proxy

    bsPxy :: Proxy B.ByteString
    bsPxy = Proxy

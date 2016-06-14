{-# LANGUAGE OverloadedStrings #-}

-- | Datatypes for law QuickChecks

-- User visible combinators. The tests are performed on pretty printing terms
-- which are constructable using the public combinators.  We need to have a
-- datatype for those combinators, otherwise it becomes almost impossible to
-- reconstruct failing tests.
--
module TestStructures (
        CDoc(..), CList(..), CDocList(..), Text(..),

        buildDoc, liftDoc2, liftDoc3, buildDocList,
        text', annotToTd, tdToStr, genericCProp
    ) where

import PrettyTestVersion
import qualified Data.Text as Text
import Data.String (fromString)

data CDoc string =
            CEmpty           -- empty
          | CText string     -- text s
          | CList CList [CDoc string] -- cat,sep,fcat,fsep ds
          | CBeside Bool (CDoc string) (CDoc string) -- a <> b and a <+> b
          | CAbove Bool (CDoc string) (CDoc string)  -- a $$ b and a $+$ b
          | CNest Int (CDoc string)   -- nest k d
    deriving (Eq, Ord)

data CList = CCat | CSep | CFCat | CFSep deriving (Eq,Ord)

newtype CDocList string = CDocList { unDocList :: [CDoc string] } 

-- wrapper for string argument of `text'
newtype Text string = Text { unText :: string } deriving (Eq, Ord, Show)

instance Show string => Show (CDoc string) where
    showsPrec k CEmpty = showString "empty"
    showsPrec k (CText s) = showParen (k >= 10) (showString " text " . shows s)
    showsPrec k (CList sp ds) = showParen (k >= 10) $ (shows sp . showList ds)
    showsPrec k (CBeside sep d1 d2) = showParen (k >= 6) $ 
        (showsPrec 6 d1) . showString (if sep then " <+> " else " <> ") . (showsPrec 6 d2) 
    showsPrec k (CAbove noOvlap d1 d2) = showParen (k >= 5) $ 
        (showsPrec 5 d1) . showString (if noOvlap then " $+$ " else " $$ ") . (showsPrec 5 d2) 
    showsPrec k (CNest n d) = showParen (k >= 10) $ showString " nest " . showsPrec 10 n . showString " ". showsPrec 10 d

instance Show CList where 
    show cs = case cs of CCat -> "cat" ;  CSep -> "sep" ; CFCat -> "fcat"  ; CFSep -> "fsep" 

instance Show string => Show (CDocList string) where show = show . unDocList
 
buildDoc :: Chars string => CDoc string -> Doc string ()
buildDoc CEmpty = empty
buildDoc (CText s) = text s
buildDoc (CList sp ds) = (listComb sp) $ fmap buildDoc ds
buildDoc (CBeside sep d1 d2) = (if sep then (<+>) else (<>)) (buildDoc d1) (buildDoc d2) 
buildDoc (CAbove noOvlap d1 d2) = (if noOvlap then ($+$) else ($$)) (buildDoc d1) (buildDoc d2) 
buildDoc (CNest k d) = nest k $ buildDoc d

listComb :: Chars string => CList -> ([Doc string ()] -> Doc string ())
listComb cs = case cs of CCat -> cat ;  CSep -> sep ; CFCat -> fcat  ; CFSep -> fsep

liftDoc2 :: Chars string => (Doc string () -> Doc string () -> a) -> (CDoc string -> CDoc string -> a)
liftDoc2 f cd1 cd2 = f (buildDoc cd1) (buildDoc cd2)

liftDoc3 :: Chars string => (Doc string () -> Doc string () -> Doc string () -> a) -> (CDoc string -> CDoc string -> CDoc string -> a)
liftDoc3 f cd1 cd2 cd3 = f (buildDoc cd1) (buildDoc cd2) (buildDoc cd3)
    
buildDocList :: Chars string => CDocList string -> [Doc string ()]
buildDocList = fmap buildDoc . unDocList

text' :: Chars string => Text string -> Doc string ()
text' (Text str) = text str

annotToTd :: Chars string => AnnotDetails string a -> TextDetails string
annotToTd (NoAnnot s _) = s
annotToTd _             = Str mempty

-- convert text details to string
tdToStr :: Chars string => TextDetails string -> string
tdToStr (Chr c) = fromString [c]
tdToStr (Str s) = s
tdToStr (PStr s) = s

-- synthesize with stop for cdoc
-- constructor order
genericCProp :: (a -> a -> a) -> (CDoc string -> (a, Bool)) -> CDoc string -> a
genericCProp c q cdoc = 
    case q cdoc of
        (v,False) -> v
        (v,True)  -> foldl c v subs
    where
        rec = genericCProp c q
        subs = case cdoc of
            CEmpty  -> []
            CText _ -> []
            CList _ ds -> fmap rec ds
            CBeside _ d1 d2 -> [rec d1, rec d2]
            CAbove b d1 d2 -> [rec d1, rec d2]
            CNest k d -> [rec d]


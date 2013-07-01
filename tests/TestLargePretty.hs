module TestLargePretty where

import Text.PrettyPrint
import Control.DeepSeq

largeDocRender :: String
largeDocRender = force $ render $ vcat $ replicate 10000000 $ text "Hello"

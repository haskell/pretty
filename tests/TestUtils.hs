-- | Test-suite framework and utility functions.
module TestUtils (
    simpleMatch
  ) where

import Control.Monad
import System.Exit
import Data.Text as Text
import Data.Text.IO as Text
import Text.PrettyPrint.Annotated.HughesPJ (Chars(..))

instance Chars Text where
    cons = Text.cons
    snoc s c = Text.append s (Text.singleton c)
    length = Text.length
    toString = unpack
    putStr = Text.putStr
    putStrLn = Text.putStrLn
    filter = Text.filter
    lines = Text.lines
    unlines = Text.unlines
    map = Text.map

simpleMatch :: String -> String -> String -> IO ()
simpleMatch test expected actual =
  when (actual /= expected) $ do
    Prelude.putStrLn $ "Test `" ++ test ++ "' failed!"
    Prelude.putStrLn "-----------------------------"
    Prelude.putStrLn $ "Expected: " ++ expected
    Prelude.putStrLn "-----------------------------"
    Prelude.putStrLn $ "Actual: " ++ actual
    Prelude.putStrLn "-----------------------------"
    exitFailure


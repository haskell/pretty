-- | Test-suite framework and utility functions.
module TestUtils (
    simpleMatch
  ) where

import Control.Monad
import System.Exit
import Data.Text as Text
import Data.Text.IO as Text
import Text.PrettyPrint.Annotated.String

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


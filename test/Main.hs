module Main (main) where

import Text.PrettyPrint
import Test.Framework (defaultMain, Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

main :: IO ()
main = defaultMain [test_one_liner]

test_one_liner :: Test
test_one_liner = testGroup "OneLinerMode" $ zipWith (testCase . show) [(1::Int)..] $ map runTest [
  (0 `here` text "start" <> 1 `here` text "stop", "startstop", [(0, Position 0 0), (1, Position 5 0)])
  ]
  where
    runTest :: (Doc Int, String, Log Int) -> Assertion
    runTest (doc, str, log) = do
      let (str', log') = renderStyleWithLog (style {mode = OneLineMode}) doc
      str @=? str'
      log @=? reverse log'

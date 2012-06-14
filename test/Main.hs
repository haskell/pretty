module Main (main) where

import Text.PrettyPrint
import Test.Framework (defaultMain, Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

main :: IO ()
main = defaultMain [test_one_liner_mode, test_left_mode, test_page_mode, test_zig_zag_mode]

type Log = [(Int, Position)]

mark :: Int -> DocL Log -> DocL Log
mark x d = here (\p -> [(x, p)]) d

test_one_liner_mode :: Test
test_one_liner_mode = enumTestGroup "OneLineMode" OneLineMode [
  (0 `mark` text "start" <> 1 `mark` text "stop", "startstop", [(0, Position 1 1), (1, Position 1 6)])
  ]

test_left_mode :: Test
test_left_mode = enumTestGroup "LeftMode" LeftMode [
    (0 `mark` text "start" <> 1 `mark` text "stop", "startstop", [(0, Position 1 1), (1, Position 1 6)])
  , (
      0 `mark` text "start" $$ 1 `mark` text "stop",
      "start\nstop",
      [(0, Position 1 1), (1, Position 2 1)]
    ) 
  ]

test_page_mode :: Test
test_page_mode = enumTestGroup "PageMode" PageMode [
    (0 `mark` text "start" <> 1 `mark` text "stop", "startstop", [(0, Position 1 1), (1, Position 1 6)])
  , (
      0 `mark` text "start" $$ 1 `mark` text "stop",
      "start\nstop",
      [(0, Position 1 1), (1, Position 2 1)]
    ) 
  ]

test_zig_zag_mode :: Test
test_zig_zag_mode = enumTestGroup "ZigZagMode" ZigZagMode [
    (0 `mark` text "start" <> 1 `mark` text "stop", "startstop", [(0, Position 1 1), (1, Position 1 6)])
  , (
      0 `mark` text "start" $$ 1 `mark` text "stop",
      "start\nstop",
      [(0, Position 1 1), (1, Position 2 1)]
    ) 
  ]

enumTestGroup :: String -> Mode -> [(DocL Log, String, Log)] -> Test -- {{{1
enumTestGroup name mode tests = testGroup name $ zipWith (testCase . show) [(1::Int)..] $ map (runTest mode) tests

runTest :: Mode -> (DocL Log, String, Log) -> Assertion -- {{{1
runTest mode (doc, str, log) = do
  let (str', log') = renderStyleWithLog (style {mode = mode}) doc
  str @=? str'
  log @=? log'


{-# OPTIONS_GHC -Wno-x-partial #-}
module Main where

import Control.Exception (evaluate)
import Control.Monad (unless)
import Control.Monad.State
import qualified Data.Text.IO as Text
import qualified Debug.TimeStats as TimeStats
import qualified Debug.TimeStats.Internal as Internal
import qualified Debug.TimeStats.Unsafe as TimeStats
import System.Environment (setEnv)
import System.Exit (exitFailure)

fib :: Int -> Int
fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
    testMeasureM
    testUnsafeMeasureM
    testFormatIntWithSeparator

testMeasureM :: IO ()
testMeasureM = do
    setEnv "DEBUG_TIMESTATS_ENABLE" "1"
    _ <- TimeStats.measureM "fib" $ evaluate (fib 21)
    _ <- TimeStats.measureM "fib2" $ evaluate (fib 20)
    _ <- TimeStats.measureM "fib2" $ evaluate (fib 19)
    xs <- TimeStats.collect
    let expected =
          [ ("fib", TimeStats.TimeStats 0 1)
          , ("fib2", TimeStats.TimeStats 0 2)
          ]
    unless (eqStats xs expected) $ do
      putStrLn "measureM: unexpected timestats:"
      Text.putStrLn (TimeStats.asText xs)
      exitFailure

testFormatIntWithSeparator :: IO ()
testFormatIntWithSeparator = do
    testCase   123456789   "123_456_789"
    testCase    23456789    "23_456_789"
    testCase     3456789     "3_456_789"
    testCase      456789       "456_789"
    testCase       56789        "56_789"
    testCase        6789         "6_789"
    testCase         789           "789"
    testCase          89            "89"
    testCase           9             "9"
    testCase           0             "0"
    testCase (-123456789) "-123_456_789"
    testCase  (-23456789)  "-23_456_789"
    testCase   (-3456789)   "-3_456_789"
    testCase    (-456789)     "-456_789"
    testCase     (-56789)      "-56_789"
    testCase      (-6789)       "-6_789"
    testCase       (-789)         "-789"
    testCase        (-89)          "-89"
    testCase         (-9)           "-9"
  where
    testCase i expected = do
      let actual = Internal.formatIntWithSeparator '_' i  "a"
      unless (actual == expected ++ "a") $ do
        putStrLn $ "unexpected output of formatIntWithSeparator:"
        putStrLn $ "expected: " ++ show (expected ++ "a")
        putStrLn $ "  actual: " ++ show actual
        exitFailure

testUnsafeMeasureM :: IO ()
testUnsafeMeasureM = do
    setEnv "DEBUG_TIMESTATS_ENABLE" "1"
    TimeStats.reset
    _ <- evaluate $ (`execState` 0) $
      TimeStats.unsafeMeasureM "fib" $ put (fib 40)
    xs <- TimeStats.collect
    let expected =
          [ ("fib", TimeStats.TimeStats 0 1)
          , ("fib2", TimeStats.TimeStats 0 0)
          ]
    -- A large time value indicates an error
    unless (eqStats xs expected
             || TimeStats.timeStat (snd $ head xs) > 1000000000) $ do
      putStrLn "unsafeMeasureM: unexpected timestats:"
      Text.putStrLn (TimeStats.asText xs)
      exitFailure

eqStats :: Eq a => [(a, TimeStats.TimeStats)] -> [(a, TimeStats.TimeStats)] -> Bool
eqStats xs ys = length xs == length ys && and (zipWith eqStat xs ys)
  where
    eqStat (lbl0, ts0) (lbl1, ts1) =
      lbl0 == lbl1 && TimeStats.countStat ts0 == TimeStats.countStat ts1

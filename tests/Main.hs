module Main where

import Control.Exception (evaluate)
import Control.Monad (unless)
import qualified Data.Text.IO as Text
import qualified Debug.TimeStats as TimeStats
import System.Environment (setEnv)
import System.Exit (exitFailure)

fib :: Int -> Int
fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
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
      putStrLn "unexpected timestats:"
      Text.putStrLn (TimeStats.asText xs)
      exitFailure
  where
    eqStats xs ys = length xs == length ys && and (zipWith eqStat xs ys)
    eqStat (lbl0, ts0) (lbl1, ts1) =
      lbl0 == lbl1 && TimeStats.countStat ts0 == TimeStats.countStat ts1

{-# LANGUAGE ScopedTypeVariables #-}

-- | A module to collect aggregates on how much time is spent in a computation
--
-- Aggregates can be identified with a label that determines where the time of
-- each computation is accounted for.
--
-- Measures are collected only if the environment variable
-- @DEBUG_TIMESTATS_ENABLE@ is set to any value ahead of invoking any function
-- in this module.
--
module Debug.TimeStats
  ( -- * Measuring
    measureM
  , measurePure
    -- * Time stats manipulation
  , printTimeStats
  , hPrintTimeStats
  , reset
  , TimeStats(..)
  , collect
  , asText
  , scope
    -- * Not intended for direct use
    --
    -- | These definitions are not intended for instrumenting applications,
    -- but they can be handy to implement other measuring primitives.
    --
  , TimeStatsRef
  , lookupTimeStatsRef
  , updateTimeStatsRef
  ) where

import Control.Exception (evaluate)
import Control.Monad (forM, forM_, unless)
import Data.IORef
import Data.Map (Map)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Word (Word64)
import Debug.TimeStats.Internal (formatIntWithSeparator)
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf (printf)
import System.Environment (lookupEnv)
import System.IO (Handle, stderr)
import System.IO.Unsafe (unsafePerformIO)


-- | Measure the time it takes to run the action.
--
-- Add the time to the stats of the given label and increase its count by one.
--
-- 'measureM' keeps the stats in a globally available store in order to minimize
-- the changes necessary when instrumenting a program. Otherwise a reference to
-- the store would need to be passed to every function that might invoke
-- functions that need this reference.
--
-- A time measure isn't collected if the given action fails with an exception.
-- This is a deliberate choice to demand less of the monad in which measures are
-- taken.
--
-- Time measures aren't collected either if the environment variable
-- @DEBUG_TIMESTATS_ENABLE@ isn't set the first time this function is
-- evaluated.
--
{-# INLINE measureM #-}
measureM :: Monad m => String -> m a -> m a
measureM label =
    -- See the documentation of 'enabled'
    if enabled then do
          -- @ref@ is the reference to the stats associated to the label.
          -- See note [Looking up stats with unsafePerformIO]
      let ref = unsafePerformIO $ lookupTimeStatsRef label
       in \action -> measureMWith ref action
    else
      id

-- | Pure version of 'measureM'. Measures the time taken to reduce the given
-- value to head normal form.
--
-- 'measurePure' is a bit dangerous to use in contexts where there are monadic
-- computations. If 'measurePure' is applied to a monadic computation it
-- will measure the time of constructing the computation rather than the time
-- of executing it, and the typechecker won't catch the mistake. We try to
-- fence against it with a longer name.
{-# INLINE measurePure #-}
measurePure :: String -> a -> a
measurePure label =
    if enabled then
      unsafePerformIO . measureM label . evaluate
    else
      id

-- Note [Looking up stats with unsafePerformIO]
--
-- When calling 'measureM' we would like to save the trouble of looking the
-- stats to update on every invocation. Hence, we use unsafePerformIO, and
-- we ask to inline 'measureM'.
--
-- Most of the time 'measureM' should be called with a statically known label.
-- When inlining, GHC should notice this fact and move the lookup closure to
-- the top-level, thus performing it only once per invocation, and perhaps
-- only once per label for all 'measureM' calls in the same module.


-- | @True@ iff the environment variable @DEBUG_TIMESTATS_ENABLE@ is set to any
-- value
--
-- We assume the value of the environment variable doesn't change during the
-- lifetime of the program.
--
-- The purpose of making this a top-level value is to have all calls to
-- 'measureM' checking it only the first time. Thus we save the trouble of
-- looking up the environment variable repeteadly.
{-# NOINLINE enabled #-}
enabled :: Bool
enabled = unsafePerformIO $ isJust <$> lookupEnv "DEBUG_TIMESTATS_ENABLE"

-- | A unique global reference to the map associating labels to their
-- stats.
{-# NOINLINE labelStatsMapRef #-}
labelStatsMapRef :: IORef (Map String TimeStatsRef)
labelStatsMapRef = unsafePerformIO $ newIORef Map.empty

-- | Set all statistics to initial values.
reset :: Monad m => m ()
reset = intersperseIOinM $
    if enabled then do
      m <- readIORef labelStatsMapRef
      forM_ (Map.elems m) $ \(TimeStatsRef ref) ->
        writeIORef ref initialTimeStats
     else
      return ()

-- | Run an action by previously reseting all stats to initial values
-- and printing them afterwards.
scope :: Monad m => m a -> m a
scope =
    if enabled then
      \m -> do
        reset
        a <- m
        hPrintTimeStats stderr
        return a
     else
      id

-- | Looks up the stats of a label. If no stats are found for the label,
-- a new TimeStatsRef is created with initial values.
--
lookupTimeStatsRef :: String -> IO TimeStatsRef
lookupTimeStatsRef label = do
    r0 <- newTimeStatsRef
    atomicModifyIORef labelStatsMapRef $ \m ->
      case Map.lookup label m of
        Nothing -> (Map.insert label r0 m, r0)
        Just r -> (m, r)

-- | Yields the labels and the stats collected thus far.
collect :: Monad m => m [(String, TimeStats)]
collect = intersperseIOinM $ do
    m <- readIORef labelStatsMapRef
    forM (Map.toList m) $ \(label, TimeStatsRef ref) ->
      (,) label <$> readIORef ref

-- | Prints the time stats to the given handle.
hPrintTimeStats :: Monad m => Handle -> m ()
hPrintTimeStats h = intersperseIOinM $ do
    xs <- collect
    unless (null xs) $
      Text.hPutStrLn h (asText xs)

-- | Prints the time stats to stderr.
printTimeStats :: Monad m => m ()
printTimeStats = hPrintTimeStats stderr

-- | Renders the given time stats in a tabular format
asText :: [(String, TimeStats)] -> Text
asText stats =
    let (lbls, timestats) = unzip stats
        (times, counts) = unzip $ map formatTimeStats timestats
        widthLbls = maximum $ map length lbls
        widthTimes = maximum $ map length times
        widthCounts = maximum $ map length counts
     in Text.unlines $
        map (Text.pack . printStat widthLbls widthTimes widthCounts) $
        zip3 lbls times counts
  where
    formatTimeStats :: TimeStats -> (String, String)
    formatTimeStats t =
      ( printf "%.3f" (fromIntegral (timeStat t) / 1e9 :: Double)
      , formatIntWithSeparator '_' (countStat t) ""
      )

    -- At the time of this writing printf can't render to 'Text'.
    printStat :: Int -> Int -> Int -> (String, String, String) -> String
    printStat widthLbls widthTimes widthCounts (label, time, count) =
      let fmt = concat
            [ "%", show widthLbls
            , "s: %", show widthTimes
            , "ss  count: %", show widthCounts, "s"
            ]
       in printf fmt (Text.pack label) time count

---------------------
-- TimeStats
---------------------

-- | A reference to a 'TimeStats' value
newtype TimeStatsRef = TimeStatsRef (IORef TimeStats)

-- | Reports how much time (in nanoseconds) the invocations to 'measureM' took
-- for a given label and how many times it was invoked on a given label.
data TimeStats = TimeStats
    { timeStat :: {-# UNPACK #-} !Word64
    , countStat :: {-# UNPACK #-} !Int
    }
  deriving Show

-- | Measured time is 0 and call count is 0.
initialTimeStats :: TimeStats
initialTimeStats = TimeStats 0 0

-- | Creates a reference to time stats with intial values
newTimeStatsRef :: Monad m => m TimeStatsRef
newTimeStatsRef = intersperseIOinM $ TimeStatsRef <$> newIORef initialTimeStats

-- | Measure the time it takes to run the given action and update with it
-- the given reference to time stats.
measureMWith :: Monad m => TimeStatsRef -> m a -> m a
measureMWith tref m = do
    t0 <- intersperseIOinM getMonotonicTimeNSec
    a <- m
    intersperseIOinM $ do
      tf <- getMonotonicTimeNSec
      updateTimeStatsRef tref $ \st ->
        st
          { timeStat = (tf - t0) + timeStat st
          , countStat = 1 + countStat st
          }
    return a

-- | Updates the TimeStats in a TimeStatsRef
updateTimeStatsRef :: TimeStatsRef -> (TimeStats -> TimeStats) -> IO ()
updateTimeStatsRef (TimeStatsRef ref) f =
    atomicModifyIORef' ref $ \st -> (f st, ())

---------------------
-- intersperseIOinM
---------------------

-- | Hack to intersperse IO actions into any monad
intersperseIOinM :: Monad m => IO a -> m a
intersperseIOinM m = do
    -- The fictitious state is only used to force @unsafePerformIO@
    -- to run @m@ every time @intersperseIOinM m@ is evaluated.
    s <- getStateM
    case unsafePerformIO $ (,) s <$> m of
      (_, r) -> pure r
  where
    -- We mark this function as NOINLINE to ensure the compiler cannot reason
    -- by unfolding that two calls of @getStateM@ yield the same value.
    {-# NOINLINE getStateM #-}
    getStateM = pure True

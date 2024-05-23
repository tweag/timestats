-- | A module with time measuring primitives that might not work in all monads
-- that building allows.
--
-- Measures are collected only if the environment variable
-- @DEBUG_TIMESTATS_ENABLE@ is set to any value ahead of invoking any function
-- in this module.
--
module Debug.TimeStats.Unsafe
  ( unsafeMeasureM
  ) where

import Debug.TimeStats
         ( TimeStats(..)
         , TimeStatsRef
         , enabled
         , lookupTimeStatsRef
         , updateTimeStatsRef
         )
import GHC.Clock (getMonotonicTimeNSec)
import System.IO.Unsafe (unsafePerformIO)

-- | Like 'Debug.TimeStats.measureM' but can measure other monads.
--
-- This function relies on a hack to perform IO in any monad, which does not
-- always work. In particular, we can expect it to miss time in monads where
--
-- > (m >>= \_ -> undefined) == undefined -- for some computation m
--
-- An example of such a monad is the list monad
--
-- > ([()] >>= \_ -> undefined) == undefined
--
-- Another example is the monad @Control.Monad.Free.Free f@.
--
-- > (Control.Monad.Free.Pure () >>= \_ -> undefined) == undefined
--
-- But it seems to work in monads with state like @IO@, @ReaderT IO@, and
-- @Control.Monad.State.State s@.
--
-- > seq (return () >>= \_ -> undefined :: YourMonadHere ()) () == ()
--
{-# INLINE unsafeMeasureM #-}
unsafeMeasureM :: Monad m => String -> m a -> m a
unsafeMeasureM label =
    if enabled then do
      let ref = unsafePerformIO $ lookupTimeStatsRef label
       in \action -> measureMWith ref action
    else
      id

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

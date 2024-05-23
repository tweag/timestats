# Revision history for timestats

## 0.1.4.1 -- 2024-05-23

* Add `Debug.TimeStats.Unsafe.unsafeMeasureM` to measure in some more monads.
* Expose `Debug.TimeStats.enabled`.

## 0.1.3 -- 2024-05-19

* Change the type of `measureM` to work with MonadIO instances only.
  [Here's](https://discourse.haskell.org/t/trick-to-lift-io-into-any-monad/9584/9)
  a discussion on why the previous interface was not obvious enough.

## 0.1.2 -- 2024-05-17

* Change the type of `measureM` to work in any monad (not only MonadIO instances).

## 0.1.1 -- 2023-11-05

* Format counts with a thousand separator

## 0.1.0 -- 2022-07-15

* First version.

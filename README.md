# timestats

This library implements time profiling by focusing on
simplicity of use. Most programs should be possible to analyze by
instrumenting the code with a few calls and then building and
running the application as usual.

This library associates fragments of a program with labels, and
measures the execution time of these fragments using the function
[getMonotonicTimeNSec](https://hackage.haskell.org/package/base-4.16.2.0/docs/GHC-Clock.html#v:getMonotonicTimeNSec).

Multiple measures of a same program fragment (or different fragments
using the same label) are aggregated and reported at chosen times of
the execution.

## Usage

```Haskell
import Control.Exception (evaluate)
import qualified Debug.TimeStats as TimeStats (printTimeStats, measureM)

fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

main = do
    -- measureM collects the time taken to compute the given action
    -- and stores it associated with a given label in global state.
    TimeStats.measureM "fib" $ evaluate (fib 31)
    -- measuring multiple times with the same label adds up
    -- the time taken by all of those invocations
    TimeStats.measureM "fib2" $ evaluate (fib 30)
    -- adds up to the existing "fib2" stats
    TimeStats.measureM "fib2" $ evaluate (fib 29)
    TimeStats.printTimeStats
```

The output when running the program with `timestats` enabled will look as

```bash
$ DEBUG_TIMESTATS_ENABLE=1 ./a.out

 fib: 2.055s  count: 1
fib2: 2.071s  count: 2
```

`timestats` is enabled by setting the environment variable
`DEBUG_TIMESTATS_ENABLE` to any value ahead of invoking any function
in [Debug.TimeStats](src/Debug/TimeStats.hs).


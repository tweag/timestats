-- | Internal functions for timestats
module Debug.TimeStats.Internal
  ( formatIntWithSeparator
  ) where


-- | Formats an int with a thousand separator.
--
-- For instance
--
-- > formatIntWithSeparator '_' 123456789 "a" == "123_456_789a"
--
formatIntWithSeparator :: Char -> Int -> ShowS
formatIntWithSeparator sep x0 s0 =
    if x0 >= 0 then go x0 s0 else '-' : go (-x0) s0
  where
    -- precondition: x >= 0
    go x s =
      let (d, m) = divMod x 1000
       in if d <= 0 then shows m s
          else go d $ sep : pad999 m ++ shows m s

    -- precondition: 0 <= m && m <= 999
    pad999 m
      | m < 10    = "00"
      | m < 100   =  "0"
      | otherwise =   ""

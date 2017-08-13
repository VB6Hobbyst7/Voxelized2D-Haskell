module Timer where

import Data.Time.Clock.POSIX

timeInMillis :: IO Integer
timeInMillis = getCurrentTime >>= pure . (1000 *) . utcTimeToPOSIXSeconds >>= pure . round

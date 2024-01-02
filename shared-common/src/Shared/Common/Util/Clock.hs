module Shared.Common.Util.Clock where

import System.Clock

fromHoursToTimeSpec :: Integer -> TimeSpec
fromHoursToTimeSpec hours = fromNanoSecs $ hours * 60 * 60 * 1000 * 1000 * 1000

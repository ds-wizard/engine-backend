module Shared.Common.Util.Uuid where

import Data.Maybe (fromJust)
import qualified Data.UUID as U
import System.Random

generateUuid :: IO U.UUID
generateUuid = randomIO

u' :: String -> U.UUID
u' = fromJust . U.fromString

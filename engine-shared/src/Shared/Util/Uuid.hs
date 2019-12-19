module Shared.Util.Uuid where

import qualified Data.UUID as U
import System.Random

generateUuid :: IO U.UUID
generateUuid = randomIO

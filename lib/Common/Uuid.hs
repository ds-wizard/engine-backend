module Common.Uuid where

import Data.Aeson
import qualified Data.UUID as U
import System.Random

generateUuid :: IO U.UUID
generateUuid = randomIO

module Wizard.Service.TemporaryFile.TemporaryFileMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Model.TemporaryFile.TemporaryFile

toTemporaryFile :: U.UUID -> String -> String -> Int -> U.UUID -> U.UUID -> UTCTime -> TemporaryFile
toTemporaryFile uuid fileName contentType expirationInSeconds appUuid createdBy now =
  TemporaryFile
    { uuid = uuid
    , fileName = fileName
    , contentType = contentType
    , expiresAt = toExpiration expirationInSeconds now
    , appUuid = appUuid
    , createdBy = createdBy
    , createdAt = now
    }

toExpiration :: Int -> UTCTime -> UTCTime
toExpiration expirationInSeconds now =
  let timeDelta = realToFrac . toInteger $ expirationInSeconds
   in addUTCTime timeDelta now

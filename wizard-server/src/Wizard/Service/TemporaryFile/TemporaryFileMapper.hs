module Wizard.Service.TemporaryFile.TemporaryFileMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.TemporaryFile.TemporaryFileDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.TemporaryFile.TemporaryFile

emptyFileDTO :: TemporaryFileDTO
emptyFileDTO =
  TemporaryFileDTO
    { url = ""
    , contentType = ""
    }

toDTO :: String -> String -> TemporaryFileDTO
toDTO url contentType =
  TemporaryFileDTO
    { url = url
    , contentType = contentType
    }

toTemporaryFile :: U.UUID -> String -> String -> Int -> U.UUID -> Maybe UserDTO -> UTCTime -> TemporaryFile
toTemporaryFile uuid fileName contentType expirationInSeconds tenantUuid mCreatedBy now =
  TemporaryFile
    { uuid = uuid
    , fileName = fileName
    , contentType = contentType
    , expiresAt = toExpiration expirationInSeconds now
    , tenantUuid = tenantUuid
    , createdBy = fmap (.uuid) mCreatedBy
    , createdAt = now
    }

toExpiration :: Int -> UTCTime -> UTCTime
toExpiration expirationInSeconds now =
  let timeDelta = realToFrac . toInteger $ expirationInSeconds
   in addUTCTime timeDelta now

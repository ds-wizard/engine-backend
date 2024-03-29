module Wizard.Service.UserToken.System.SystemMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Date
import Wizard.Model.User.User
import WizardLib.Public.Model.User.UserToken
import WizardLib.Public.Service.UserToken.UserTokenMapper

fromSystemDTO :: U.UUID -> User -> Integer -> String -> Maybe String -> Maybe String -> UTCTime -> String -> UserToken
fromSystemDTO uuid user expiration secret mUserAgent mSessionState now tokenValue =
  let timeDelta = realToFrac $ expiration * nominalHourInSeconds
      expiresAt = addUTCTime timeDelta now
   in toUserToken uuid "Token" LoginUserTokenType user.uuid expiresAt secret mUserAgent mSessionState user.tenantUuid now tokenValue

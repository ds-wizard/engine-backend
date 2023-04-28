module Wizard.Service.UserToken.Login.LoginMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Model.User.User
import Wizard.Service.UserToken.UserTokenMapper
import Wizard.Util.Date
import WizardLib.Public.Model.User.UserToken

fromLoginDTO :: U.UUID -> User -> Integer -> String -> Maybe String -> Maybe String -> UTCTime -> String -> UserToken
fromLoginDTO uuid user expiration secret mUserAgent mSessionState now tokenValue =
  let timeDelta = realToFrac $ expiration * nominalHourInSeconds
      expiresAt = addUTCTime timeDelta now
   in toUserToken uuid "Token" LoginUserTokenType user.uuid expiresAt secret mUserAgent mSessionState user.appUuid now tokenValue

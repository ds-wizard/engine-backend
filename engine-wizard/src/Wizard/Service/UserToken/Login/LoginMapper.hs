module Wizard.Service.UserToken.Login.LoginMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Model.User.User
import Wizard.Model.User.UserToken
import Wizard.Service.UserToken.UserTokenMapper
import Wizard.Util.Date

fromLoginDTO :: U.UUID -> User -> Integer -> String -> Maybe String -> Maybe String -> UTCTime -> UserToken
fromLoginDTO uuid user expiration secret mUserAgent mSessionState now =
  let timeDelta = realToFrac $ expiration * nominalHourInSeconds
      expiresAt = addUTCTime timeDelta now
   in toUserToken uuid "Token" LoginUserTokenType user.uuid expiresAt secret mUserAgent mSessionState user.appUuid now

module Wizard.Service.UserToken.UserTokenMapper where

import Data.Maybe (fromMaybe)
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.UUID as U
import qualified Jose.Jwt as JWT

import Shared.Common.Util.Date
import Wizard.Constant.UserToken
import Wizard.Model.Config.ServerConfig
import Wizard.Model.User.User
import WizardLib.Public.Api.Resource.UserToken.UserTokenClaimsDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Model.User.UserToken
import WizardLib.Public.Model.User.UserTokenList

toDTO :: UserToken -> UserTokenDTO
toDTO token = UserTokenDTO {token = token.value, expiresAt = token.expiresAt}

toList :: UserToken -> Bool -> UserTokenList
toList token currentSession =
  UserTokenList
    { uuid = token.uuid
    , name = token.name
    , userAgent = token.userAgent
    , currentSession = currentSession
    , expiresAt = token.expiresAt
    , createdAt = token.createdAt
    }

toUserToken :: U.UUID -> String -> UserTokenType -> U.UUID -> UTCTime -> String -> Maybe String -> Maybe String -> U.UUID -> UTCTime -> String -> UserToken
toUserToken uuid name tokenType userUuid expiresAt secret mUserAgent mSessionState appUuid now tokenValue =
  UserToken
    { uuid = uuid
    , name = name
    , tType = tokenType
    , userUuid = userUuid
    , value = tokenValue
    , userAgent = fromMaybe "Unknown User Agent" mUserAgent
    , sessionState = mSessionState
    , expiresAt = expiresAt
    , appUuid = appUuid
    , createdAt = now
    }

toUserTokenClaims :: User -> U.UUID -> UTCTime -> ServerConfigJwt -> UserTokenClaimsDTO
toUserTokenClaims user tokenUuid now config =
  let timeDelta = realToFrac $ config.expiration * nominalHourInSeconds
   in toUserTokenClaimsWithExpiration user tokenUuid now (addUTCTime timeDelta now)

toUserTokenClaimsWithExpiration :: User -> U.UUID -> UTCTime -> UTCTime -> UserTokenClaimsDTO
toUserTokenClaimsWithExpiration user tokenUuid now expiresAt =
  UserTokenClaimsDTO
    { exp = JWT.IntDate $ utcTimeToPOSIXSeconds expiresAt
    , version = userTokenVersion
    , tokenUuid = tokenUuid
    , userUuid = user.uuid
    }
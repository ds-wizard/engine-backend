module WizardLib.Public.Service.UserToken.UserTokenMapper where

import Data.Maybe (fromMaybe)
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.UUID as U
import qualified Jose.Jwt as JWT

import Shared.Common.Util.Date
import WizardLib.Public.Api.Resource.UserToken.UserTokenClaimsDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Constant.UserToken
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
toUserToken uuid name tokenType userUuid expiresAt secret mUserAgent mSessionState tenantUuid now tokenValue =
  UserToken
    { uuid = uuid
    , name = name
    , tType = tokenType
    , userUuid = userUuid
    , value = tokenValue
    , userAgent = fromMaybe "Unknown User Agent" mUserAgent
    , sessionState = mSessionState
    , expiresAt = expiresAt
    , tenantUuid = tenantUuid
    , createdAt = now
    }

toUserTokenClaims :: U.UUID -> U.UUID -> UTCTime -> Integer -> UserTokenClaimsDTO
toUserTokenClaims userUuid tokenUuid now expiration =
  let timeDelta = realToFrac $ expiration * nominalHourInSeconds
   in toUserTokenClaimsWithExpiration userUuid tokenUuid now (addUTCTime timeDelta now)

toUserTokenClaimsWithExpiration :: U.UUID -> U.UUID -> UTCTime -> UTCTime -> UserTokenClaimsDTO
toUserTokenClaimsWithExpiration userUuid tokenUuid now expiresAt =
  UserTokenClaimsDTO
    { exp = JWT.IntDate $ utcTimeToPOSIXSeconds expiresAt
    , version = userTokenVersion
    , tokenUuid = tokenUuid
    , userUuid = userUuid
    }

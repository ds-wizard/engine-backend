module Wizard.Service.UserToken.UserTokenMapper where

import Data.Aeson
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as U
import qualified Web.JWT as JWT

import Wizard.Api.Resource.UserToken.UserTokenDTO
import Wizard.Constant.UserToken
import Wizard.Model.User.UserToken
import Wizard.Model.User.UserTokenList
import Wizard.Service.UserToken.UserTokenUtil
import Wizard.Util.Date

toDTO :: UserToken -> UserTokenDTO
toDTO token = UserTokenDTO {token = token.value}

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

toUserToken :: U.UUID -> String -> UserTokenType -> U.UUID -> UTCTime -> String -> Maybe String -> Maybe String -> U.UUID -> UTCTime -> UserToken
toUserToken uuid name tokenType userUuid expiresAt secret mUserAgent mSessionState appUuid now =
  let cs =
        JWT.JWTClaimsSet
          { iss = Nothing
          , sub = Nothing
          , aud = Nothing
          , exp = toNumericDate expiresAt
          , nbf = Nothing
          , iat = Nothing
          , jti = Nothing
          , unregisteredClaims =
              JWT.ClaimsMap
                { unClaimsMap =
                    M.insert "version" (String . T.pack . show $ userTokenVersion)
                      . M.insert "tokenUuid" (toJSON uuid)
                      . M.insert "userUuid" (toJSON userUuid)
                      $ M.empty
                }
          }
      tokenValue = signToken secret cs
   in UserToken
        { uuid = uuid
        , name = name
        , tType = tokenType
        , userUuid = userUuid
        , value = tokenValue
        , userAgent = fromMaybe "Uknown User Agent" mUserAgent
        , sessionState = mSessionState
        , expiresAt = expiresAt
        , appUuid = appUuid
        , createdAt = now
        }

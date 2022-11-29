module Wizard.Service.UserToken.UserTokenMapper where

import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as U
import qualified Web.JWT as JWT

import Wizard.Api.Resource.UserToken.UserTokenDTO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.User.User
import Wizard.Model.User.UserToken
import Wizard.Service.UserToken.UserTokenUtil
import Wizard.Util.Date

toDTO :: UserToken -> UserTokenDTO
toDTO token = UserTokenDTO {token = token.value}

toUserToken :: User -> U.UUID -> Maybe String -> UTCTime -> ServerConfigJwt -> String -> UserToken
toUserToken user tokenUuid mSessionState now config secret =
  let timeDelta = realToFrac $ config.expiration * nominalDayInSeconds
      cs =
        JWT.JWTClaimsSet
          { iss = Nothing
          , sub = Nothing
          , aud = Nothing
          , exp = toNumericDate (addUTCTime timeDelta now)
          , nbf = Nothing
          , iat = Nothing
          , jti = Nothing
          , unregisteredClaims =
              JWT.ClaimsMap
                { unClaimsMap =
                    M.insert "version" (String . T.pack . show $ config.version)
                      . M.insert "tokenUuid" (toJSON tokenUuid)
                      . M.insert "userUuid" (toJSON user.uuid)
                      $ M.empty
                }
          }
      tokenValue = signToken secret cs
   in UserToken
        { uuid = tokenUuid
        , userUuid = user.uuid
        , value = tokenValue
        , sessionState = mSessionState
        , appUuid = user.appUuid
        , createdAt = user.lastVisitedAt
        }

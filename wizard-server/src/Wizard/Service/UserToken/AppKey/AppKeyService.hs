module Wizard.Service.UserToken.AppKey.AppKeyService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Jose.Jwt as JWT

import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.User
import Wizard.Service.UserToken.AppKey.AppKeyMapper
import WizardLib.Public.Api.Resource.UserToken.AppKeyCreateDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Database.DAO.User.UserTokenDAO
import WizardLib.Public.Service.UserToken.UserTokenMapper
import WizardLib.Public.Service.UserToken.UserTokenUtil

createAppKey :: AppKeyCreateDTO -> Maybe String -> AppContextM UserTokenDTO
createAppKey reqDto mUserAgent =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    uuid <- liftIO generateUuid
    userDto <- getCurrentUser
    user <- findUserByUuid userDto.uuid
    tenantUuid <- asks currentTenantUuid
    now <- liftIO getCurrentTime
    let expiresAt = createExpiresAt now
    let claims = toUserTokenClaimsWithExpiration user.uuid uuid user.tenantUuid now expiresAt
    (JWT.Jwt jwtToken) <- createSignedJwtToken claims
    let userToken = fromAppKeyDTO reqDto uuid user.uuid expiresAt serverConfig.general.secret mUserAgent tenantUuid now (BS.unpack jwtToken)
    insertUserToken userToken
    return . toDTO $ userToken

createExpiresAt :: UTCTime -> UTCTime
createExpiresAt now =
  let timeDelta = realToFrac $ 99999 * nominalHourInSeconds
   in addUTCTime timeDelta now

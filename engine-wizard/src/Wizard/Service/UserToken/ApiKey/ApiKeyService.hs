module Wizard.Service.UserToken.ApiKey.ApiKeyService where

import Control.Monad.Reader (asks, liftIO)
import Data.Time

import Shared.Util.Uuid
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.UserToken.ApiKeyCreateDTO
import Wizard.Api.Resource.UserToken.UserTokenDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserTokenDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Service.UserToken.ApiKey.ApiKeyMapper
import Wizard.Service.UserToken.UserTokenMapper

createApiKey :: ApiKeyCreateDTO -> Maybe String -> AppContextM UserTokenDTO
createApiKey reqDto mUserAgent =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    uuid <- liftIO generateUuid
    user <- getCurrentUser
    appUuid <- asks currentAppUuid
    now <- liftIO getCurrentTime
    let userToken = fromApiKeyDTO reqDto uuid user.uuid serverConfig.general.secret mUserAgent appUuid now
    insertUserToken userToken
    return . toDTO $ userToken

module Wizard.Service.App.AppService where

import Control.Lens ((.~), (^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Util.Uuid
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Config.AppConfigDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.App.App
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.AppConfigDM
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import Wizard.Model.User.UserDM
import Wizard.Service.App.AppMapper

getCurrentApp :: AppContextM App
getCurrentApp =
  runInTransaction $ do
    aUuid <- asks _appContextAppUuid
    findAppById (U.toString aUuid)

getAppClientUrl :: AppContextM String
getAppClientUrl = do
  serverConfig <- asks _appContextServerConfig
  if serverConfig ^. experimental . moreAppsEnabled
    then do
      app <- getCurrentApp
      return $ app ^. clientUrl
    else return $ serverConfig ^. general . clientUrl

createApp :: AppCreateDTO -> AppContextM App
createApp reqDto = do
  runInTransaction $ do
    aUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let app = fromCreate reqDto aUuid now
    insertApp app
    createUser aUuid now
    createAppConfig aUuid now
    return app

-- --------------------------------
-- PRIVATE
-- --------------------------------
createAppConfig :: U.UUID -> UTCTime -> AppContextM AppConfig
createAppConfig aUuid now = do
  runInTransaction $ do
    let appConfig = (uuid .~ aUuid) . (createdAt .~ now) . (updatedAt .~ now) $ defaultAppConfig
    insertAppConfig appConfig
    return appConfig

createUser :: U.UUID -> UTCTime -> AppContextM User
createUser aUuid now =
  runInTransaction $ do
    uUuid <- liftIO generateUuid
    let user =
          (uuid .~ uUuid) . (appUuid .~ aUuid) . (lastVisitedAt .~ now) . (createdAt .~ now) . (updatedAt .~ now) $
          defaultUser
    insertUser user
    return user

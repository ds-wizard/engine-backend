module Wizard.Service.App.AppService where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import qualified Data.UUID as U

import LensesConfig
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.Common
import Wizard.Model.App.App
import Wizard.Model.Context.AppContext

getCurrentApp :: AppContextM App
getCurrentApp =
  runInTransaction $ do
    appUuid <- asks _appContextAppUuid
    findAppById (U.toString appUuid)

getAppClientUrl :: AppContextM String
getAppClientUrl = do
  serverConfig <- asks _appContextServerConfig
  if serverConfig ^. experimental . moreAppsEnabled
    then do
      app <- getCurrentApp
      return $ app ^. clientUrl
    else return $ serverConfig ^. general . clientUrl

module Wizard.Service.App.AppHelper where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U

import Shared.Model.Config.ServerConfig
import Wizard.Database.DAO.App.AppDAO
import Wizard.Model.App.App
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext

getCurrentApp :: AppContextM App
getCurrentApp = do
  aUuid <- asks currentAppUuid
  findAppById (U.toString aUuid)

getAppClientUrl :: AppContextM String
getAppClientUrl = do
  serverConfig <- asks serverConfig
  if serverConfig.cloud.enabled
    then do
      app <- getCurrentApp
      return app.clientUrl
    else return serverConfig.general.clientUrl

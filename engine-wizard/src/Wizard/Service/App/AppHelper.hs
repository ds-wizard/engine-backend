module Wizard.Service.App.AppHelper where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import qualified Data.UUID as U

import LensesConfig
import Wizard.Database.DAO.App.AppDAO
import Wizard.Model.App.App
import Wizard.Model.Context.AppContext

getCurrentApp :: AppContextM App
getCurrentApp = do
  aUuid <- asks _appContextAppUuid
  findAppById (U.toString aUuid)

getAppClientUrl :: AppContextM String
getAppClientUrl = do
  serverConfig <- asks _appContextServerConfig
  if serverConfig ^. cloud . enabled
    then do
      app <- getCurrentApp
      return $ app ^. clientUrl
    else return $ serverConfig ^. general . clientUrl

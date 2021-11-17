module Wizard.Service.App.AppService where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U

import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.Common
import Wizard.Model.App.App
import Wizard.Model.Context.AppContext

getCurrentApp :: AppContextM App
getCurrentApp =
  runInTransaction $ do
    appUuid <- asks _appContextAppUuid
    findAppById (U.toString appUuid)

module Wizard.Service.PersistentCommand.PersistentCommandUtil where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig hiding (hash)
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.PersistentCommand.PersistentCommand
import Wizard.Service.App.AppUtil
import Wizard.Service.PersistentCommand.PersistentCommandMapper

enhancePersistentCommand :: PersistentCommand -> AppContextM PersistentCommandDTO
enhancePersistentCommand command = do
  mUser <-
    case command ^. createdBy of
      Just userUuid -> findUserByIdSystem' (U.toString userUuid)
      Nothing -> return Nothing
  app <- findAppById (U.toString $ command ^. appUuid)
  appDto <- enhanceApp app
  return $ toDTO command mUser appDto

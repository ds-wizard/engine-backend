module Wizard.Service.PersistentCommand.PersistentCommandUtil where

import qualified Data.UUID as U

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.App.AppUtil
import Wizard.Service.PersistentCommand.PersistentCommandMapper

enhancePersistentCommand :: PersistentCommand U.UUID -> AppContextM PersistentCommandDTO
enhancePersistentCommand command = do
  mUser <-
    case command.createdBy of
      Just userUuid -> findUserByUuidSystem' userUuid
      Nothing -> return Nothing
  app <- findAppByUuid command.appUuid
  appDto <- enhanceApp app
  return $ toDTO command mUser appDto

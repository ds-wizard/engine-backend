module Wizard.Service.PersistentCommand.PersistentCommandService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandChangeDTO
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommandSimple
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandService
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Mapping.ActionKey.ActionKeyType ()
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Service.App.AppUtil
import Wizard.Service.PersistentCommand.PersistentCommandExecutor
import Wizard.Service.PersistentCommand.PersistentCommandMapper
import Wizard.Service.PersistentCommand.PersistentCommandUtil
import qualified Wizard.Service.User.UserMapper as UM
import Wizard.Util.Context

getPersistentCommandsPage :: [String] -> Pageable -> [Sort] -> AppContextM (Page PersistentCommandDTO)
getPersistentCommandsPage states pageable sort = do
  checkPermission _DEV_PERM
  commands <- findPersistentCommandsPage states pageable sort
  traverse enhancePersistentCommand commands

getPersistentCommandById :: U.UUID -> AppContextM PersistentCommandDetailDTO
getPersistentCommandById uuid = do
  checkPermission _DEV_PERM
  command <- findPersistentCommandByUuid uuid
  mUser <-
    case command.createdBy of
      Just userUuid -> findUserByUuidSystem' userUuid
      Nothing -> return Nothing
  app <- findAppByUuid command.appUuid
  appDto <- enhanceApp app
  return $ toDetailDTO command mUser appDto

createPersistentCommand :: PersistentCommand U.UUID -> AppContextM (PersistentCommand U.UUID)
createPersistentCommand persistentCommand =
  runInTransaction $ do
    checkPermission _DEV_PERM
    mPersistentCommandFromDb <- findPersistentCommandByUuid' persistentCommand.uuid :: AppContextM (Maybe (PersistentCommand U.UUID))
    case mPersistentCommandFromDb of
      Just _ -> return persistentCommand
      Nothing -> do
        insertPersistentCommand persistentCommand
        return persistentCommand

modifyPersistentCommand :: U.UUID -> PersistentCommandChangeDTO -> AppContextM PersistentCommandDetailDTO
modifyPersistentCommand uuid reqDto = do
  checkPermission _DEV_PERM
  command <- findPersistentCommandByUuid uuid
  now <- liftIO getCurrentTime
  let updatedCommand = fromChangeDTO command reqDto now :: PersistentCommand U.UUID
  updatePersistentCommandByUuid updatedCommand
  getPersistentCommandById uuid

runPersistentCommandById :: U.UUID -> AppContextM PersistentCommandDetailDTO
runPersistentCommandById uuid = do
  command <- findPersistentCommandByUuid uuid
  if command.internal
    then runPersistentCommand' True (toSimple command)
    else do
      notifySpecificPersistentCommandQueue command
      return ()
  getPersistentCommandById uuid

runPersistentCommands' :: AppContextM ()
runPersistentCommands' = runPersistentCommands runAppContextWithAppContext updateContext createPersistentCommand execute

runPersistentCommand' :: Bool -> PersistentCommandSimple U.UUID -> AppContextM ()
runPersistentCommand' = runPersistentCommand runAppContextWithAppContext updateContext createPersistentCommand execute

runPersistentCommandChannelListener' :: AppContextM ()
runPersistentCommandChannelListener' = runPersistentCommandChannelListener runAppContextWithAppContext updateContext createPersistentCommand execute

updateContext :: PersistentCommandSimple U.UUID -> AppContext -> AppContextM AppContext
updateContext commandSimple context = do
  user <-
    case commandSimple.createdBy of
      Just userUuid -> findUserByUuidSystem' userUuid
      Nothing -> return Nothing
  return $
    context
      { currentAppUuid = commandSimple.appUuid
      , currentUser = fmap UM.toDTO user
      }

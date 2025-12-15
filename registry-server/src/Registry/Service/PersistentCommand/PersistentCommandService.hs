module Registry.Service.PersistentCommand.PersistentCommandService where

import Control.Monad.Except (throwError)

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.AppContextHelpers
import Registry.Model.Context.ContextMappers
import Registry.Service.PersistentCommand.PersistentCommandExecutor
import RegistryLib.Model.Organization.Organization
import RegistryLib.Model.Organization.OrganizationRole
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommandSimple
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandService

createPersistentCommand :: PersistentCommand String -> AppContextM (PersistentCommand String)
createPersistentCommand persistentCommand =
  runInTransaction $ do
    checkPermissionToCreatePersistentCommand
    mPersistentCommandFromDb <- findPersistentCommandByUuid' persistentCommand.uuid :: AppContextM (Maybe (PersistentCommand String))
    case mPersistentCommandFromDb of
      Just _ -> return persistentCommand
      Nothing -> do
        insertPersistentCommand persistentCommand
        return persistentCommand

runPersistentCommands' :: AppContextM ()
runPersistentCommands' = runPersistentCommands runAppContextWithAppContext' updateContext emptyTransferFn execute

runPersistentCommandChannelListener' :: AppContextM ()
runPersistentCommandChannelListener' = runPersistentCommandChannelListener runAppContextWithAppContext' updateContext emptyTransferFn execute

emptyTransferFn :: String -> PersistentCommand String -> AppContextM ()
emptyTransferFn _ _ = return ()

updateContext :: PersistentCommandSimple String -> AppContext -> AppContextM AppContext
updateContext commandSimple = return

-- --------------------------------
-- PERMISSIONS
-- --------------------------------
checkPermissionToCreatePersistentCommand = do
  currentOrg <- getCurrentOrganization
  if currentOrg.oRole == AdminRole
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Create Persistent Command"

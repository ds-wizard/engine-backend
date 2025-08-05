module Wizard.Service.Migration.Metamodel.MigratorCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.Model.PersistentCommand.Migration.Metamodel.MigrateToLatestMetamodelVersionCommand
import Wizard.Service.Migration.Metamodel.MigratorService

cComponent = "metamodel_migrator"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cMigrateName = cMigrate command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cMigrateName = "migrate"

cMigrate :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cMigrate persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String MigrateToLatestMetamodelVersionCommand
  case eCommand of
    Right command -> do
      migrateTenant
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

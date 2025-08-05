module Shared.Prefab.Service.Prefab.PrefabCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Model.Context.AppContext
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.Prefab.Model.PersistentCommand.Prefab.CreateOrUpdatePrefabCommand
import Shared.Prefab.Model.PersistentCommand.Prefab.DeletePrefabCommand
import Shared.Prefab.Service.Prefab.PrefabService

cComponent = "prefab"

execute :: AppContextC s sc m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreatePrefabName = cCreatePrefab command
  | command.function == cUpdatePrefabName = cUpdatePrefab command
  | command.function == cDeletePrefabName = cDeletePrefab command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cCreatePrefabName = "createPrefab"

cCreatePrefab :: AppContextC s sc m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
cCreatePrefab persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdatePrefabCommand
  case eCommand of
    Right command -> do
      createPrefab command
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdatePrefabName = "updatePrefab"

cUpdatePrefab :: AppContextC s sc m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
cUpdatePrefab persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdatePrefabCommand
  case eCommand of
    Right command -> do
      modifyPrefab command
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cDeletePrefabName = "deletePrefab"

cDeletePrefab :: AppContextC s sc m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
cDeletePrefab persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String DeletePrefabCommand
  case eCommand of
    Right command -> do
      deletePrefab command.uuid
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

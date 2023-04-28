module Wizard.Service.App.AppCommandExecutor where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL

import Wizard.Api.Resource.App.AppChangeDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.PersistentCommand.PersistentCommand
import Wizard.Service.App.AppService
import Wizard.Util.Logger
import WizardLib.Public.Model.PersistentCommand.App.CreateOrUpdateAppCommand

cComponent = "app"

execute :: PersistentCommand -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreateAppName = cCreateApp command
  | command.function == cUpdateAppName = cUpdateApp command

cCreateAppName = "createApp"

cCreateApp :: PersistentCommand -> AppContextM (PersistentCommandState, Maybe String)
cCreateApp persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdateAppCommand
  case eCommand of
    Right command -> do
      createAppByCommand command
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdateAppName = "updateApp"

cUpdateApp :: PersistentCommand -> AppContextM (PersistentCommandState, Maybe String)
cUpdateApp persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdateAppCommand
  case eCommand of
    Right command -> do
      let reqDto = AppChangeDTO {appId = command.appId, name = command.name}
      modifyApp command.uuid reqDto
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

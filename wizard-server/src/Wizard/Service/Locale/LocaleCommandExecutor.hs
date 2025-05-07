module Wizard.Service.Locale.LocaleCommandExecutor where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.S3.Locale.LocaleS3
import WizardLib.Public.Model.PersistentCommand.Trigger.TriggerEntityIdCommand

cComponent = "locale"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cDeleteFromS3Name = cDeleteFromS3 command

cDeleteFromS3Name = "deleteFromS3"

cDeleteFromS3 :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cDeleteFromS3 persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String TriggerEntityIdCommand
  case eCommand of
    Right command -> do
      removeLocale command.aId
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

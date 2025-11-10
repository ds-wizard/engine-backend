module Wizard.Service.Document.DocumentCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.S3.Document.DocumentS3
import WizardLib.Public.Model.PersistentCommand.Trigger.TriggerEntityUuidCommand

cComponent = "document"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cDeleteFromS3Name = cDeleteFromS3 command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cDeleteFromS3Name = "deleteFromS3"

cDeleteFromS3 :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cDeleteFromS3 persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String TriggerEntityUuidCommand
  case eCommand of
    Right command -> do
      removeDocumentContent command.uuid
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

module Wizard.Service.Questionnaire.File.QuestionnaireFileCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.Model.PersistentCommand.Questionnaire.File.QuestionnaireFileDeleteFromS3Command
import Wizard.S3.Questionnaire.QuestionnaireFileS3

cComponent = "questionnaire_file"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cDeleteFromS3Name = cDeleteFromS3 command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cDeleteFromS3Name = "deleteFromS3"

cDeleteFromS3 :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cDeleteFromS3 persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String QuestionnaireFileDeleteFromS3Command
  case eCommand of
    Right command -> do
      removeFile command.questionnaireUuid command.fileUuid
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

module Wizard.Service.Questionnaire.QuestionnaireCommandExecutor where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.Service.Questionnaire.QuestionnaireService
import WizardLib.Public.Model.PersistentCommand.Questionnaire.CreateQuestionnaireCommand

cComponent = "questionnaire"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreateQuestionnairesName = cCreateQuestionnaires command

cCreateQuestionnairesName = "createQuestionnaires"

cCreateQuestionnaires :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cCreateQuestionnaires persistentCommand = do
  let eCommands = eitherDecode (BSL.pack persistentCommand.body) :: Either String [CreateQuestionnaireCommand]
  case eCommands of
    Right commands -> do
      createQuestionnairesFromCommands commands
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

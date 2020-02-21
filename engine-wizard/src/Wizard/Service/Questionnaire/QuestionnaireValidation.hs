module Wizard.Service.Questionnaire.QuestionnaireValidation where

import Control.Monad.Except (throwError)

import Shared.Model.Error.Error
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

validateQuestionnaireDeletation :: String -> AppContextM ()
validateQuestionnaireDeletation = validateUsageByQtnMigration

validateUsageByQtnMigration :: String -> AppContextM ()
validateUsageByQtnMigration qtnUuid = do
  result <- findMigratorStatesByOldQuestionnaireId qtnUuid
  case result of
    [] -> return ()
    _ -> throwError . UserError $ _ERROR_SERVICE_QTN__QTN_CANT_BE_DELETED_BECAUSE_IT_IS_USED_IN_MIGRATION

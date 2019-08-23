module Service.Questionnaire.QuestionnaireValidation where

import Database.DAO.Migration.Questionnaire.MigratorDAO
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Model.Error.ErrorHelpers
import Util.Helper (createHmmHelper)

validateQuestionnaireDeletation :: String -> AppContextM (Maybe AppError)
validateQuestionnaireDeletation qtnUuid = validateUsageByQtnMigration qtnUuid

validateUsageByQtnMigration :: String -> AppContextM (Maybe AppError)
validateUsageByQtnMigration qtnUuid = do
  eitherResult <- findMigratorStatesByOldQuestionnaireId qtnUuid
  case eitherResult of
    Right [] -> return Nothing
    Right _ ->
      return . Just . createErrorWithErrorMessage $
      _ERROR_SERVICE_QTN__QTN_CANT_BE_DELETED_BECAUSE_IT_IS_USED_IN_MIGRATION
    Left error -> return . Just $ error

-- --------------------------------
-- HELPERS
-- --------------------------------
hmValidateQuestionnaireDeletation qtnUuid callback = createHmmHelper (validateQuestionnaireDeletation qtnUuid) callback

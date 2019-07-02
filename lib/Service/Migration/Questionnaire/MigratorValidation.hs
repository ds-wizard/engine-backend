module Service.Migration.Questionnaire.MigratorValidation
  ( validateQuestionnaireMigrationNotExist
  -- Helpers
  , heValidateQuestionnaireMigrationNotExist
  ) where

import Database.DAO.Migration.Questionnaire.MigratorDAO
import Localization
import Model.Context.AppContext
import Model.Error.Error

validateQuestionnaireMigrationNotExist :: String -> AppContextM (Maybe AppError)
validateQuestionnaireMigrationNotExist qtnUuid = do
  state <- findMigratorStateByOldQuestionnaireId qtnUuid
  case state of
    Right _ -> return . Just $ MigratorError _ERROR_SERVICE_MIGRATION_QTN__MIGRATION_UNIQUENESS
    Left (NotExistsError _) -> return Nothing
    Left error -> return . Just $ error

-- --------------------------------
-- HELPERS
-- --------------------------------
heValidateQuestionnaireMigrationNotExist qtnUuid callback = do
  maybeError <- validateQuestionnaireMigrationNotExist qtnUuid
  case maybeError of
    Nothing -> callback
    Just error -> return . Left $ error

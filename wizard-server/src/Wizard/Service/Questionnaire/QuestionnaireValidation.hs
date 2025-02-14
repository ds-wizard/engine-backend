module Wizard.Service.Questionnaire.QuestionnaireValidation where

import Control.Monad.Except (throwError)
import Data.Foldable (forM_, traverse_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Text.Regex.TDFA

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Questionnaire.QuestionnaireSettingsChangeDTO
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

validateQuestionnaireSettingsChangeDTO :: QuestionnaireSettingsChangeDTO -> AppContextM ()
validateQuestionnaireSettingsChangeDTO reqDto = validateQuestionnaireTags reqDto.projectTags

validateQuestionnaireDeletation :: U.UUID -> AppContextM ()
validateQuestionnaireDeletation = validateUsageByQtnMigration

validateUsageByQtnMigration :: U.UUID -> AppContextM ()
validateUsageByQtnMigration qtnUuid = do
  result <- findMigratorStatesByOldQuestionnaireUuid qtnUuid
  case result of
    [] -> return ()
    _ -> throwError . UserError $ _ERROR_SERVICE_QTN__QTN_CANT_BE_DELETED_BECAUSE_IT_IS_USED_IN_MIGRATION

validateQuestionnaireTags :: [String] -> AppContextM ()
validateQuestionnaireTags = traverse_ validateQuestionnaireTag

validateQuestionnaireTag :: String -> AppContextM ()
validateQuestionnaireTag tag = forM_ (isValidProjectTag tag) throwError

isValidProjectTag :: String -> Maybe AppError
isValidProjectTag tag =
  if tag =~ "^[^,]+$"
    then Nothing
    else Just $ ValidationError [] (M.singleton "tags" [_ERROR_VALIDATION__FORBIDDEN_CHARACTERS tag])

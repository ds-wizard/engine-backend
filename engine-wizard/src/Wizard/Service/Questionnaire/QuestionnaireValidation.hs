module Wizard.Service.Questionnaire.QuestionnaireValidation where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Data.Foldable (forM_, traverse_)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Text.Regex (matchRegex, mkRegex)

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

validateQuestionnaireChangeDTO :: QuestionnaireChangeDTO -> AppContextM ()
validateQuestionnaireChangeDTO reqDto = validateQuestionnaireTags (reqDto ^. projectTags)

validateQuestionnaireDeletation :: String -> AppContextM ()
validateQuestionnaireDeletation = validateUsageByQtnMigration

validateUsageByQtnMigration :: String -> AppContextM ()
validateUsageByQtnMigration qtnUuid = do
  result <- findMigratorStatesByOldQuestionnaireId qtnUuid
  case result of
    [] -> return ()
    _ -> throwError . UserError $ _ERROR_SERVICE_QTN__QTN_CANT_BE_DELETED_BECAUSE_IT_IS_USED_IN_MIGRATION

validateQuestionnaireTags :: [String] -> AppContextM ()
validateQuestionnaireTags = traverse_ validateQuestionnaireTag

validateQuestionnaireTag :: String -> AppContextM ()
validateQuestionnaireTag tag = forM_ (isValidProjectTag tag) throwError

isValidProjectTag :: String -> Maybe AppError
isValidProjectTag tag =
  if isJust $ matchRegex validationRegex tag
    then Nothing
    else Just $ ValidationError [] (M.singleton "tags" [_ERROR_VALIDATION__FORBIDDEN_CHARACTERS tag])
  where
    validationRegex = mkRegex "^[^,]+$"

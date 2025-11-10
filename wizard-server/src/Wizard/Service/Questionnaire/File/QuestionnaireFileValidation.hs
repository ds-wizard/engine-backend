module Wizard.Service.Questionnaire.File.QuestionnaireFileValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireFile
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Tenant.Limit.LimitService

validateQuestionnaireFile :: Questionnaire -> U.UUID -> QuestionnaireFile -> AppContextM ()
validateQuestionnaireFile qtn questionUuid qtnFile = do
  checkStorageSize qtnFile.fileSize
  km <- compileKnowledgeModel [] (Just qtn.knowledgeModelPackageId) qtn.selectedQuestionTagUuids
  case M.lookup questionUuid (getQuestionsM km) of
    Just (FileQuestion' question) ->
      case question.maxSize of
        (Just maxFileSize) ->
          when
            (maxFileSize < fromIntegral qtnFile.fileSize)
            (throwError . UserError $ _ERROR_VALIDATION__QUESTIONNAIRE_FILE_SIZE_EXCEEDS_LIMIT qtnFile.fileSize maxFileSize)
        Nothing -> return ()
    _ -> throwError . UserError $ _ERROR_VALIDATION__QUESTIONNAIRE_FILE_QUESTION_ABSENCE_OR_WRONG_TYPE

module Wizard.Service.Project.File.ProjectFileValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.File.ProjectFile
import Wizard.Model.Project.Project
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Tenant.Limit.LimitService

validateProjectFile :: Project -> U.UUID -> ProjectFile -> AppContextM ()
validateProjectFile project questionUuid projectFile = do
  checkStorageSize projectFile.fileSize
  km <- compileKnowledgeModel [] (Just project.knowledgeModelPackageId) project.selectedQuestionTagUuids
  case M.lookup questionUuid (getQuestionsM km) of
    Just (FileQuestion' question) ->
      case question.maxSize of
        (Just maxFileSize) ->
          when
            (maxFileSize < fromIntegral projectFile.fileSize)
            (throwError . UserError $ _ERROR_VALIDATION__PROJECT_FILE_SIZE_EXCEEDS_LIMIT projectFile.fileSize maxFileSize)
        Nothing -> return ()
    _ -> throwError . UserError $ _ERROR_VALIDATION__PROJECT_FILE_QUESTION_ABSENCE_OR_WRONG_TYPE

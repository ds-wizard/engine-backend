module Wizard.Service.DocumentTemplate.DocumentTemplateValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import qualified Data.UUID as U
import GHC.Records

import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import WizardLib.Common.Service.Coordinate.CoordinateValidation
import WizardLib.DocumentTemplate.Constant.DocumentTemplate
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import WizardLib.DocumentTemplate.Localization.Messages.Public
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

validateNewDocumentTemplate :: DocumentTemplate -> Bool -> AppContextM ()
validateNewDocumentTemplate tml shouldValidateMetamodelVersion = do
  validateCoordinateFormat False "templateId" tml.tId
  validateDocumentTemplateIdUniqueness tml.tId
  validateCoordinateWithParams tml.tId tml.organizationId tml.templateId tml.version
  when shouldValidateMetamodelVersion (validateMetamodelVersion tml)

validateChangeDto :: String -> DocumentTemplateChangeDTO -> AppContextM ()
validateChangeDto tmlId reqDto = validatePhase tmlId reqDto.phase

validatePhase :: String -> DocumentTemplatePhase -> AppContextM ()
validatePhase tmlId newPhase = do
  when
    (newPhase == DraftDocumentTemplatePhase)
    (throwError . UserError $ _ERROR_VALIDATION__DOC_TML_UNSUPPORTED_STATE tmlId (show newPhase))

validateExistingDocumentTemplate :: DocumentTemplate -> AppContextM ()
validateExistingDocumentTemplate tml = do
  validateCoordinateFormat False "templateId" tml.tId
  validateCoordinateWithParams tml.tId tml.organizationId tml.templateId tml.version

validateDocumentTemplateIdUniqueness :: String -> AppContextM ()
validateDocumentTemplateIdUniqueness tmlId = do
  mTml <- findDocumentTemplateById' tmlId
  case mTml of
    Nothing -> return ()
    Just _ -> throwError . UserError $ _ERROR_VALIDATION__DOC_TML_ID_UNIQUENESS tmlId

validateDocumentTemplateDeletation :: String -> AppContextM ()
validateDocumentTemplateDeletation tmlId = do
  validateUsageBySomeQuestionnaire tmlId
  validateUsageBySomeDocument tmlId

validateUsageBySomeQuestionnaire :: String -> AppContextM ()
validateUsageBySomeQuestionnaire tmlId = do
  questionnaires <- findQuestionnairesByDocumentTemplateId tmlId
  case questionnaires of
    [] -> return ()
    _ ->
      throwError . UserError $
        _ERROR_VALIDATION__TML_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY tmlId "questionnaire"

validateUsageBySomeDocument :: String -> AppContextM ()
validateUsageBySomeDocument tmlId = do
  questionnaires <- findDocumentsByDocumentTemplateId tmlId
  case questionnaires of
    [] -> return ()
    _ ->
      throwError . UserError $
        _ERROR_VALIDATION__TML_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY tmlId "document"

validateMetamodelVersion :: DocumentTemplate -> AppContextM ()
validateMetamodelVersion tml =
  when
    (tml.metamodelVersion /= documentTemplateMetamodelVersion)
    ( throwError . UserError $
        _ERROR_VALIDATION__TEMPLATE_UNSUPPORTED_METAMODEL_VERSION tml.tId tml.metamodelVersion documentTemplateMetamodelVersion
    )

validateFileAndAssetUniqueness :: Maybe U.UUID -> String -> String -> AppContextM ()
validateFileAndAssetUniqueness mTemplateEntityUuid templateId fileName = do
  files <- findFilesByDocumentTemplateIdAndFileName templateId fileName
  assets <- findAssetsByDocumentTemplateIdAndFileName templateId fileName
  checkUniqueness files
  checkUniqueness assets
  where
    checkUniqueness :: HasField "uuid" entity U.UUID => [entity] -> AppContextM ()
    checkUniqueness arrays =
      case arrays of
        [] -> return ()
        [entity] ->
          if mTemplateEntityUuid == Just entity.uuid
            then return ()
            else throwError . UserError $ _ERROR_VALIDATION__DOC_TML_FILE_OR_ASSET_UNIQUENESS

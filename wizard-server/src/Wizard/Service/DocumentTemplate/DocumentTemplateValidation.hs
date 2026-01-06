module Wizard.Service.DocumentTemplate.DocumentTemplateValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import qualified Data.UUID as U
import GHC.Records

import Shared.Common.Model.Error.Error
import Shared.Coordinate.Service.Coordinate.CoordinateValidation
import Shared.DocumentTemplate.Constant.DocumentTemplate
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.DocumentTemplate.Localization.Messages.Public
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

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

validateDocumentTemplateDeletion :: String -> AppContextM ()
validateDocumentTemplateDeletion tmlId = do
  validateUsageBySomeProject tmlId
  validateUsageBySomeDocument tmlId

validateUsageBySomeProject :: String -> AppContextM ()
validateUsageBySomeProject tmlId = do
  projects <- findProjectsByDocumentTemplateId tmlId
  case projects of
    [] -> return ()
    _ ->
      throwError . UserError $
        _ERROR_VALIDATION__TML_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY tmlId "project"

validateUsageBySomeDocument :: String -> AppContextM ()
validateUsageBySomeDocument tmlId = do
  projects <- findDocumentsByDocumentTemplateId tmlId
  case projects of
    [] -> return ()
    _ ->
      throwError . UserError $
        _ERROR_VALIDATION__TML_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY tmlId "document"

validateMetamodelVersion :: DocumentTemplate -> AppContextM ()
validateMetamodelVersion tml =
  when
    (isDocumentTemplateUnsupported tml.metamodelVersion)
    ( throwError . UserError $
        _ERROR_VALIDATION__TEMPLATE_UNSUPPORTED_METAMODEL_VERSION tml.tId (show tml.metamodelVersion) (show documentTemplateMetamodelVersion)
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
        _ -> throwError . UserError $ _ERROR_VALIDATION__DOC_TML_FILE_OR_ASSET_UNIQUENESS

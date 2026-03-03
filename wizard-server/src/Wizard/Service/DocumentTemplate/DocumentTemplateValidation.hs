module Wizard.Service.DocumentTemplate.DocumentTemplateValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import qualified Data.UUID as U
import GHC.Records

import Shared.Common.Model.Error.Error
import Shared.Coordinate.Model.Coordinate.Coordinate
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
validateNewDocumentTemplate dt shouldValidateMetamodelVersion = do
  validateDocumentTemplateIdUniqueness (createCoordinate dt)
  when shouldValidateMetamodelVersion (validateMetamodelVersion dt)

validateChangeDto :: U.UUID -> DocumentTemplateChangeDTO -> AppContextM ()
validateChangeDto uuid reqDto = validatePhase uuid reqDto.phase

validatePhase :: U.UUID -> DocumentTemplatePhase -> AppContextM ()
validatePhase uuid newPhase = do
  when
    (newPhase == DraftDocumentTemplatePhase)
    (throwError . UserError $ _ERROR_VALIDATION__DOC_TML_UNSUPPORTED_STATE (U.toString uuid) (show newPhase))

validateDocumentTemplateIdUniqueness :: Coordinate -> AppContextM ()
validateDocumentTemplateIdUniqueness coordinate = do
  mDt <- findDocumentTemplateByCoordinate' coordinate
  case mDt of
    Nothing -> return ()
    Just _ -> throwError . UserError $ _ERROR_VALIDATION__DOC_TML_ID_UNIQUENESS (show coordinate)

validateDocumentTemplateDeletion :: U.UUID -> AppContextM ()
validateDocumentTemplateDeletion dtUuid = do
  validateUsageBySomeProject dtUuid
  validateUsageBySomeDocument dtUuid

validateUsageBySomeProject :: U.UUID -> AppContextM ()
validateUsageBySomeProject dtUuid = do
  projects <- findProjectsByDocumentTemplateUuid dtUuid
  case projects of
    [] -> return ()
    _ ->
      throwError . UserError $
        _ERROR_VALIDATION__TML_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY (U.toString dtUuid) "project"

validateUsageBySomeDocument :: U.UUID -> AppContextM ()
validateUsageBySomeDocument dtUuid = do
  projects <- findDocumentsByDocumentTemplateUuid dtUuid
  case projects of
    [] -> return ()
    _ ->
      throwError . UserError $
        _ERROR_VALIDATION__TML_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY (U.toString dtUuid) "document"

validateMetamodelVersion :: DocumentTemplate -> AppContextM ()
validateMetamodelVersion dt =
  when
    (isDocumentTemplateUnsupported dt.metamodelVersion)
    ( throwError . UserError $
        _ERROR_VALIDATION__TEMPLATE_UNSUPPORTED_METAMODEL_VERSION (show . createCoordinate $ dt) (show dt.metamodelVersion) (show documentTemplateMetamodelVersion)
    )

validateFileAndAssetUniqueness :: Maybe U.UUID -> U.UUID -> String -> AppContextM ()
validateFileAndAssetUniqueness mTemplateEntityUuid dtUuid fileName = do
  files <- findFilesByDocumentTemplateUuidAndFileName dtUuid fileName
  assets <- findAssetsByDocumentTemplateUuidAndFileName dtUuid fileName
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

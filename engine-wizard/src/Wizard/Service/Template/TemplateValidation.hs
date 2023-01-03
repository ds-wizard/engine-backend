module Wizard.Service.Template.TemplateValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import qualified Data.UUID as U
import GHC.Records

import Shared.Constant.Template
import Shared.Database.DAO.Template.TemplateAssetDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Database.DAO.Template.TemplateFileDAO
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Shared.Service.Coordinate.CoordinateValidation
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Service.Config.App.AppConfigService

validateNewTemplate :: Template -> AppContextM ()
validateNewTemplate tml = do
  validateCoordinateFormat False tml.tId
  validateTemplateIdUniqueness tml.tId
  validateCoordinateWithParams tml.tId tml.organizationId tml.templateId tml.version
  validateMetamodelVersion tml

validateExistingTemplate :: Template -> AppContextM ()
validateExistingTemplate tml = do
  validateCoordinateFormat False tml.tId
  validateCoordinateWithParams tml.tId tml.organizationId tml.templateId tml.version

validateTemplateIdUniqueness :: String -> AppContextM ()
validateTemplateIdUniqueness tmlId = do
  mTml <- findTemplateById' tmlId
  case mTml of
    Nothing -> return ()
    Just _ -> throwError . UserError $ _ERROR_VALIDATION__TML_ID_UNIQUENESS tmlId

validateTemplateDeletation :: String -> AppContextM ()
validateTemplateDeletation tmlId = do
  validateUsageByAppConfig tmlId
  validateUsageBySomeQuestionnaire tmlId
  validateUsageBySomeDocument tmlId

validateUsageByAppConfig :: String -> AppContextM ()
validateUsageByAppConfig tmlId = do
  appConfig <- getAppConfig
  case appConfig.template.recommendedTemplateId of
    (Just appConfigtmlId) ->
      when
        (tmlId == appConfigtmlId)
        ( throwError . UserError $
            _ERROR_VALIDATION__TML_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY
              tmlId
              "application configuration"
        )
    _ -> return ()

validateUsageBySomeQuestionnaire :: String -> AppContextM ()
validateUsageBySomeQuestionnaire tmlId = do
  questionnaires <- findQuestionnairesByTemplateId tmlId
  case questionnaires of
    [] -> return ()
    _ ->
      throwError . UserError $
        _ERROR_VALIDATION__TML_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY tmlId "questionnaire"

validateUsageBySomeDocument :: String -> AppContextM ()
validateUsageBySomeDocument tmlId = do
  questionnaires <- findDocumentsByTemplateId tmlId
  case questionnaires of
    [] -> return ()
    _ ->
      throwError . UserError $
        _ERROR_VALIDATION__TML_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY tmlId "document"

validateMetamodelVersion :: Template -> AppContextM ()
validateMetamodelVersion tml =
  when
    (tml.metamodelVersion /= templateMetamodelVersion)
    ( throwError . UserError $
        _ERROR_VALIDATION__TEMPLATE_UNSUPPORTED_VERSION tml.tId tml.metamodelVersion templateMetamodelVersion
    )

validateTemplateFileAndAssetUniqueness :: Maybe U.UUID -> String -> String -> AppContextM ()
validateTemplateFileAndAssetUniqueness mTemplateEntityUuid templateId fileName = do
  templateFiles <- findTemplateFilesByTemplateIdAndFileName templateId fileName
  templateAssets <- findTemplateAssetsByTemplateIdAndFileName templateId fileName
  checkUniqueness templateFiles
  checkUniqueness templateAssets
  where
    checkUniqueness :: HasField "uuid" entity U.UUID => [entity] -> AppContextM ()
    checkUniqueness arrays =
      case arrays of
        [] -> return ()
        [entity] ->
          if mTemplateEntityUuid == Just entity.uuid
            then return ()
            else throwError . UserError $ _ERROR_SERVICE_TML__TML_FILE_OR_ASSET_UNIQUENESS

module Wizard.Service.Template.TemplateValidation where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Except (throwError)

import LensesConfig
import Shared.Constant.Template
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Coordinate.CoordinateValidation

validateNewTemplate :: Template -> AppContextM ()
validateNewTemplate tml = do
  validateCoordinateFormat (tml ^. tId)
  validateTemplateIdUniqueness (tml ^. tId)
  validateCoordinateWithParams (tml ^. tId) (tml ^. organizationId) (tml ^. templateId) (tml ^. version)
  validateMetamodelVersion tml

validateExistingTemplate :: Template -> AppContextM ()
validateExistingTemplate tml = do
  validateCoordinateFormat (tml ^. tId)
  validateCoordinateWithParams (tml ^. tId) (tml ^. organizationId) (tml ^. templateId) (tml ^. version)

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
  case appConfig ^. template . recommendedTemplateId of
    (Just appConfigtmlId) ->
      when
        (tmlId == appConfigtmlId)
        (throwError . UserError $
         _ERROR_VALIDATION__TML_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY
           tmlId
           "application configuration")
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
    (tml ^. metamodelVersion /= templateMetamodelVersion)
    (throwError . UserError $
     _ERROR_VALIDATION__TEMPLATE_UNSUPPORTED_VERSION (tml ^. tId) (tml ^. metamodelVersion) templateMetamodelVersion)

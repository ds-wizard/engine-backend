module Wizard.Service.Template.TemplateValidation where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Except (throwError)

import LensesConfig
import Shared.Constant.Template
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.AppConfigService

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

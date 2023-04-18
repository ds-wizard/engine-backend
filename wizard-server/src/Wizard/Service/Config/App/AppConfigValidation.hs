module Wizard.Service.Config.App.AppConfigValidation where

import Control.Monad.Except (throwError)
import Data.Foldable (forM_)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Text.Regex (matchRegex, mkRegex)

import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.Questionnaire.QuestionnaireValidation
import WizardLib.Common.Localization.Messages.Public

validateAppConfig :: AppConfigChangeDTO -> AppContextM ()
validateAppConfig appConfig = do
  validateOrganization appConfig.organization
  validateQuestionnaire appConfig.questionnaire

validateOrganization :: AppConfigOrganization -> AppContextM ()
validateOrganization config = forM_ (isValidOrganizationId config.organizationId) throwError

isValidOrganizationId :: String -> Maybe AppError
isValidOrganizationId kmId =
  if isJust $ matchRegex validationRegex kmId
    then Nothing
    else Just $ ValidationError [] (M.singleton "organizationId" [_ERROR_VALIDATION__INVALID_ORG_ID_FORMAT])
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9.]*[a-zA-Z0-9]$"

validateQuestionnaire :: AppConfigQuestionnaire -> AppContextM ()
validateQuestionnaire config = validateQuestionnaireTags config.projectTagging.tags

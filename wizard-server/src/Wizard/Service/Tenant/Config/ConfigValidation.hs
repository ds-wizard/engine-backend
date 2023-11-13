module Wizard.Service.Tenant.Config.ConfigValidation where

import Control.Monad.Except (throwError)
import Data.Foldable (forM_)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Text.Regex (matchRegex, mkRegex)

import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Questionnaire.QuestionnaireValidation
import WizardLib.Common.Localization.Messages.Public

validateTenantConfig :: TenantConfigChangeDTO -> AppContextM ()
validateTenantConfig tenantConfig = do
  validateOrganization tenantConfig.organization
  validateQuestionnaire tenantConfig.questionnaire

validateOrganization :: TenantConfigOrganization -> AppContextM ()
validateOrganization config = forM_ (isValidOrganizationId config.organizationId) throwError

isValidOrganizationId :: String -> Maybe AppError
isValidOrganizationId kmId =
  if isJust $ matchRegex validationRegex kmId
    then Nothing
    else Just $ ValidationError [] (M.singleton "organizationId" [_ERROR_VALIDATION__INVALID_ORG_ID_FORMAT])
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9.]*[a-zA-Z0-9]$"

validateQuestionnaire :: TenantConfigQuestionnaire -> AppContextM ()
validateQuestionnaire config = validateQuestionnaireTags config.projectTagging.tags

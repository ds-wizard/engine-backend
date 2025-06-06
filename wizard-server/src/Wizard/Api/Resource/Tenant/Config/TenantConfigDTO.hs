module Wizard.Api.Resource.Tenant.Config.TenantConfigDTO where

import GHC.Generics

import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Config.TenantConfigSubmission
import WizardLib.Public.Model.Tenant.Config.TenantConfig

data TenantConfigDTO = TenantConfigDTO
  { organization :: TenantConfigOrganization
  , authentication :: TenantConfigAuth
  , privacyAndSupport :: TenantConfigPrivacyAndSupport
  , dashboardAndLoginScreen :: TenantConfigDashboardAndLoginScreen
  , lookAndFeel :: TenantConfigLookAndFeel
  , registry :: TenantConfigRegistry
  , knowledgeModel :: TenantConfigKnowledgeModel
  , questionnaire :: TenantConfigQuestionnaire
  , submission :: TenantConfigSubmission
  }
  deriving (Show, Eq, Generic)

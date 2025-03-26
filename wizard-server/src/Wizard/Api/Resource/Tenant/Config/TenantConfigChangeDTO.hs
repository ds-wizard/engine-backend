module Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO where

import GHC.Generics

import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Model.Tenant.Config.TenantConfig

data TenantConfigChangeDTO = TenantConfigChangeDTO
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
  deriving (Generic, Show)

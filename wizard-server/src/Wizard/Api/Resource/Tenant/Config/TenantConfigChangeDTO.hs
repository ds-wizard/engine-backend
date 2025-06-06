module Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
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
  , submission :: TenantConfigSubmissionChangeDTO
  }
  deriving (Generic, Show)

data TenantConfigSubmissionChangeDTO = TenantConfigSubmissionChangeDTO
  { enabled :: Bool
  , services :: [TenantConfigSubmissionServiceChangeDTO]
  }
  deriving (Generic, Eq, Show)

data TenantConfigSubmissionServiceChangeDTO = TenantConfigSubmissionServiceChangeDTO
  { sId :: String
  , name :: String
  , description :: String
  , props :: [String]
  , supportedFormats :: [TenantConfigSubmissionServiceSupportedFormatChangeDTO]
  , request :: TenantConfigSubmissionServiceRequestChangeDTO
  }
  deriving (Generic, Eq, Show)

data TenantConfigSubmissionServiceSupportedFormatChangeDTO = TenantConfigSubmissionServiceSupportedFormatChangeDTO
  { templateId :: String
  , formatUuid :: U.UUID
  }
  deriving (Generic, Eq, Show)

data TenantConfigSubmissionServiceRequestChangeDTO = TenantConfigSubmissionServiceRequestChangeDTO
  { method :: String
  , url :: String
  , headers :: M.Map String String
  , multipart :: TenantConfigSubmissionServiceRequestMultipartChangeDTO
  }
  deriving (Generic, Eq, Show)

data TenantConfigSubmissionServiceRequestMultipartChangeDTO = TenantConfigSubmissionServiceRequestMultipartChangeDTO
  { enabled :: Bool
  , fileName :: String
  }
  deriving (Generic, Eq, Show)

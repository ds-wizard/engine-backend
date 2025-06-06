module Wizard.Model.Tenant.Config.TenantConfigSubmission where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data TenantConfigSubmission = TenantConfigSubmission
  { tenantUuid :: U.UUID
  , enabled :: Bool
  , services :: [TenantConfigSubmissionService]
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigSubmission where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.enabled == b.enabled
      && a.services == b.services

data TenantConfigSubmissionService = TenantConfigSubmissionService
  { tenantUuid :: U.UUID
  , sId :: String
  , name :: String
  , description :: String
  , props :: [String]
  , supportedFormats :: [TenantConfigSubmissionServiceSupportedFormat]
  , request :: TenantConfigSubmissionServiceRequest
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigSubmissionService where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.sId == b.sId
      && a.name == b.name
      && a.description == b.description
      && a.props == b.props
      && a.supportedFormats == b.supportedFormats
      && a.request == b.request

data TenantConfigSubmissionServiceSupportedFormat = TenantConfigSubmissionServiceSupportedFormat
  { tenantUuid :: U.UUID
  , serviceId :: String
  , templateId :: String
  , formatUuid :: U.UUID
  }
  deriving (Generic, Eq, Show)

data TenantConfigSubmissionServiceRequest = TenantConfigSubmissionServiceRequest
  { method :: String
  , url :: String
  , headers :: M.Map String String
  , multipart :: TenantConfigSubmissionServiceRequestMultipart
  }
  deriving (Generic, Eq, Show)

data TenantConfigSubmissionServiceRequestMultipart = TenantConfigSubmissionServiceRequestMultipart
  { enabled :: Bool
  , fileName :: String
  }
  deriving (Generic, Eq, Show)

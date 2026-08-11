module WizardLib.Public.Model.PersistentCommand.Tenant.CreateTenantCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson
import WizardLib.Public.Model.Tenant.Limit.TenantLimitBundleChange

data CreateTenantCommand = CreateTenantCommand
  { uuid :: U.UUID
  , tenantId :: String
  , name :: String
  , enabled :: Bool
  , customDomain :: Maybe String
  , adminRoleUuid :: U.UUID
  , adminRolePermissions :: [String]
  , limits :: TenantLimitBundleChange
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateTenantCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateTenantCommand where
  toJSON = genericToJSON jsonOptions

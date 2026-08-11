module WizardLib.Public.Model.PersistentCommand.Tenant.UpdateTenantCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson
import WizardLib.Public.Model.Tenant.Limit.TenantLimitBundleChange

data UpdateTenantCommand = UpdateTenantCommand
  { uuid :: U.UUID
  , tenantId :: String
  , name :: String
  , enabled :: Bool
  , customDomain :: Maybe String
  , limits :: TenantLimitBundleChange
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdateTenantCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdateTenantCommand where
  toJSON = genericToJSON jsonOptions

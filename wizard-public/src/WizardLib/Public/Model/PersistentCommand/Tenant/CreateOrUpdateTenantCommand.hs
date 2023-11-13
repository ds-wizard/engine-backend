module WizardLib.Public.Model.PersistentCommand.Tenant.CreateOrUpdateTenantCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data CreateOrUpdateTenantCommand = CreateOrUpdateTenantCommand
  { uuid :: U.UUID
  , tenantId :: String
  , name :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateOrUpdateTenantCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateOrUpdateTenantCommand where
  toJSON = genericToJSON jsonOptions

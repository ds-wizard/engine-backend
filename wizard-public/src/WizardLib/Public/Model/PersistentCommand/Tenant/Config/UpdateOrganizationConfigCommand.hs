module WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateOrganizationConfigCommand where

import Data.Aeson
import GHC.Generics

import Shared.Common.Util.Aeson

data UpdateOrganizationConfigCommand = UpdateOrganizationConfigCommand
  { name :: String
  , description :: String
  , organizationId :: String
  , affiliations :: [String]
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdateOrganizationConfigCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdateOrganizationConfigCommand where
  toJSON = genericToJSON jsonOptions

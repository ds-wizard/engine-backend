module WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateSupportConfigCommand where

import Data.Aeson
import GHC.Generics

import Shared.Common.Util.Aeson

data UpdateSupportConfigCommand = UpdateSupportConfigCommand
  { supportEmail :: Maybe String
  , supportSiteName :: Maybe String
  , supportSiteUrl :: Maybe String
  , supportSiteIcon :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdateSupportConfigCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdateSupportConfigCommand where
  toJSON = genericToJSON jsonOptions

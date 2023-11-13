module WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdatePrivacyAndSupportConfigCommand where

import Data.Aeson
import GHC.Generics

import Shared.Common.Util.Aeson

data UpdatePrivacyAndSupportConfigCommand = UpdatePrivacyAndSupportConfigCommand
  { privacyUrl :: Maybe String
  , termsOfServiceUrl :: Maybe String
  , supportEmail :: Maybe String
  , supportSiteName :: Maybe String
  , supportSiteUrl :: Maybe String
  , supportSiteIcon :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdatePrivacyAndSupportConfigCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdatePrivacyAndSupportConfigCommand where
  toJSON = genericToJSON jsonOptions

module WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateLookAndFeelConfigCommand where

import Data.Aeson
import GHC.Generics

import Shared.Common.Util.Aeson

data UpdateLookAndFeelConfigCommand = UpdateLookAndFeelConfigCommand
  { logoUrl :: Maybe String
  , primaryColor :: Maybe String
  , illustrationsColor :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdateLookAndFeelConfigCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdateLookAndFeelConfigCommand where
  toJSON = genericToJSON jsonOptions

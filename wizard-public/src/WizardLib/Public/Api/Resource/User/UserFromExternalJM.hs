module WizardLib.Public.Api.Resource.User.UserFromExternalJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.User.UserFromExternalDTO

instance FromJSON UserFromExternalDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserFromExternalDTO where
  toJSON = genericToJSON jsonOptions

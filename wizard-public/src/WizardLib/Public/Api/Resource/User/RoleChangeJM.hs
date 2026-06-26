module WizardLib.Public.Api.Resource.User.RoleChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.User.RoleChangeDTO

instance FromJSON RoleChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RoleChangeDTO where
  toJSON = genericToJSON jsonOptions

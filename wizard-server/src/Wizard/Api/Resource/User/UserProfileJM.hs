module Wizard.Api.Resource.User.UserProfileJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.User.UserProfile
import WizardLib.Public.Api.Resource.User.RoleSimpleJM ()

instance FromJSON UserProfile where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserProfile where
  toJSON = genericToJSON jsonOptions

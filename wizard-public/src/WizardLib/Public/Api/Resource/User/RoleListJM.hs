module WizardLib.Public.Api.Resource.User.RoleListJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Model.User.RoleList

instance FromJSON RoleList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RoleList where
  toJSON = genericToJSON jsonOptions

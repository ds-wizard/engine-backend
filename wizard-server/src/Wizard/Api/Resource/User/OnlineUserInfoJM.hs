module Wizard.Api.Resource.User.OnlineUserInfoJM where

import Data.Aeson
import WizardLib.Public.Api.Resource.User.RoleSimpleJM ()

import Shared.Common.Util.Aeson
import Wizard.Model.User.OnlineUserInfo

instance FromJSON OnlineUserInfo where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON OnlineUserInfo where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

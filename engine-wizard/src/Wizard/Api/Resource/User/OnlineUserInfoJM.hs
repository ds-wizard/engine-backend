module Wizard.Api.Resource.User.OnlineUserInfoJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Model.User.OnlineUserInfo

instance FromJSON OnlineUserInfo where
  parseJSON = genericParseJSON (createSimpleOptions'''' "OnlineUserInfo")

instance ToJSON OnlineUserInfo where
  toJSON = genericToJSON (createSimpleOptions'''' "OnlineUserInfo")

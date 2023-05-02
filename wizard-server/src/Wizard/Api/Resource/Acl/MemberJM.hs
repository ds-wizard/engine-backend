module Wizard.Api.Resource.Acl.MemberJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Acl.MemberDTO
import Wizard.Model.Acl.Acl

instance FromJSON Member where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON Member where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON MemberDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON MemberDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

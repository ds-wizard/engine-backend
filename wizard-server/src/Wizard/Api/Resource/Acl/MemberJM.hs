module Wizard.Api.Resource.Acl.MemberJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Acl.MemberDTO

instance FromJSON MemberDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON MemberDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

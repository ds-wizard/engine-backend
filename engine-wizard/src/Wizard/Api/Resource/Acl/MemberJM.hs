module Wizard.Api.Resource.Acl.MemberJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Acl.MemberDTO
import Wizard.Model.Acl.Acl

instance FromJSON Member where
  parseJSON = genericParseJSON (createSimpleOptions'''' "Member")

instance ToJSON Member where
  toJSON = genericToJSON (createSimpleOptions'''' "Member")

instance FromJSON MemberDTO where
  parseJSON = genericParseJSON (createSimpleOptions'''' "MemberDTO")

instance ToJSON MemberDTO where
  toJSON = genericToJSON (createSimpleOptions'''' "MemberDTO")

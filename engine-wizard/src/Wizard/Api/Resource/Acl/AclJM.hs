module Wizard.Api.Resource.Acl.AclJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Model.Acl.Acl

instance FromJSON GroupMembershipType

instance ToJSON GroupMembershipType

instance FromJSON GroupMembership where
  parseJSON = simpleParseJSON "_groupMembership"

instance ToJSON GroupMembership where
  toJSON = simpleToJSON "_groupMembership"

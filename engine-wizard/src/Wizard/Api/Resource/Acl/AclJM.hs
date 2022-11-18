module Wizard.Api.Resource.Acl.AclJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Model.Acl.Acl

instance FromJSON GroupMembershipType

instance ToJSON GroupMembershipType

instance FromJSON GroupMembership where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON GroupMembership where
  toJSON = genericToJSON jsonOptions

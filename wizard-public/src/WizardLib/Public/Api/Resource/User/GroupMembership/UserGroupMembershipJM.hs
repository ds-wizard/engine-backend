module WizardLib.Public.Api.Resource.User.GroupMembership.UserGroupMembershipJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Model.User.UserGroupMembership

instance FromJSON UserGroupMembershipType where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserGroupMembershipType where
  toJSON = genericToJSON jsonOptions

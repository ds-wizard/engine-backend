module WizardLib.Public.Api.Resource.User.UserWithMembershipJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.User.GroupMembership.UserGroupMembershipJM ()
import WizardLib.Public.Api.Resource.User.UserWithMembershipDTO

instance FromJSON UserWithMembershipDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserWithMembershipDTO where
  toJSON = genericToJSON jsonOptions

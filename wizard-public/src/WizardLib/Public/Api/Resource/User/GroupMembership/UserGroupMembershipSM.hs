module WizardLib.Public.Api.Resource.User.GroupMembership.UserGroupMembershipSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.User.GroupMembership.UserGroupMembershipJM ()
import WizardLib.Public.Model.User.UserGroupMembership

instance ToSchema UserGroupMembershipType where
  declareNamedSchema = toSwagger OwnerUserGroupMembershipType

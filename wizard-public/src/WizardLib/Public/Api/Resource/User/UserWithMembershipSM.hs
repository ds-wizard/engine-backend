module WizardLib.Public.Api.Resource.User.UserWithMembershipSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.User.GroupMembership.UserGroupMembershipSM ()
import WizardLib.Public.Api.Resource.User.UserWithMembershipDTO
import WizardLib.Public.Api.Resource.User.UserWithMembershipJM ()
import WizardLib.Public.Database.Migration.Development.User.Data.Users

instance ToSchema UserWithMembershipDTO where
  declareNamedSchema = toSwagger userAlbertWithMembership

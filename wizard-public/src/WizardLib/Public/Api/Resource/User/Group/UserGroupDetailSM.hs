module WizardLib.Public.Api.Resource.User.Group.UserGroupDetailSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.User.Group.UserGroupDetailDTO
import WizardLib.Public.Api.Resource.User.Group.UserGroupDetailJM ()
import WizardLib.Public.Api.Resource.User.UserWithMembershipSM ()
import WizardLib.Public.Database.Migration.Development.User.Data.UserGroups
import WizardLib.Public.Service.User.Group.UserGroupMapper

instance ToSchema UserGroupDetailDTO where
  declareNamedSchema = toSwagger (toDetailDTO bioGroup [])

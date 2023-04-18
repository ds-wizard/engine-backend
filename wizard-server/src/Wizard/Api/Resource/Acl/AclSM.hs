module Wizard.Api.Resource.Acl.AclSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Acl.AclJM ()
import Wizard.Database.Migration.Development.Acl.Data.Groups
import Wizard.Model.Acl.Acl

instance ToSchema GroupMembershipType

instance ToSchema GroupMembership where
  declareNamedSchema = toSwagger memberBioGroup

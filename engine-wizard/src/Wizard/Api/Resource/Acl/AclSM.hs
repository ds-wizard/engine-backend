module Wizard.Api.Resource.Acl.AclSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Acl.AclJM ()
import Wizard.Database.Migration.Development.Acl.Data.Groups
import Wizard.Model.Acl.Acl

instance ToSchema GroupMembershipType

instance ToSchema GroupMembership where
  declareNamedSchema = simpleToSchema' "_groupMembership" memberBioGroup

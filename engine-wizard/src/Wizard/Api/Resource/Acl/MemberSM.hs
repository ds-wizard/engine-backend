module Wizard.Api.Resource.Acl.MemberSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Acl.MemberDTO
import Wizard.Api.Resource.Acl.MemberJM ()
import Wizard.Database.Migration.Development.Acl.Data.Members
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Acl.Acl
import Wizard.Service.Acl.AclMapper

instance ToSchema Member where
  declareNamedSchema = simpleToSchema' "_member" bioGroupMember

instance ToSchema MemberDTO where
  declareNamedSchema = simpleToSchema' "_memberDTO" (toUserMemberDTO userAlbert)

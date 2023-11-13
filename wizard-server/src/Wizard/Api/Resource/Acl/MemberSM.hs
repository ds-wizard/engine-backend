module Wizard.Api.Resource.Acl.MemberSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Acl.MemberDTO
import Wizard.Api.Resource.Acl.MemberJM ()
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Service.Acl.AclMapper

instance ToSchema MemberDTO where
  declareNamedSchema = toSwagger (toUserMemberDTO userAlbert)

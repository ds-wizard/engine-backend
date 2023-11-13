module Wizard.Service.Acl.AclMapper where

import Shared.Common.Util.Gravatar
import Wizard.Api.Resource.Acl.MemberDTO
import Wizard.Model.User.User
import WizardLib.Public.Model.User.UserGroup

toUserMemberDTO :: User -> MemberDTO
toUserMemberDTO user =
  UserMemberDTO
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , gravatarHash = createGravatarHash $ user.email
    , imageUrl = user.imageUrl
    }

toUserGroupMemberDTO :: UserGroup -> MemberDTO
toUserGroupMemberDTO userGroup =
  UserGroupMemberDTO
    { uuid = userGroup.uuid
    , name = userGroup.name
    , description = userGroup.description
    , private = userGroup.private
    }

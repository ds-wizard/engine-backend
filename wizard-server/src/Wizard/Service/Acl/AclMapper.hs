module Wizard.Service.Acl.AclMapper where

import Shared.Common.Util.Gravatar
import Wizard.Api.Resource.Acl.MemberDTO
import Wizard.Model.Acl.Acl
import Wizard.Model.User.User

toUserMemberDTO :: User -> MemberDTO
toUserMemberDTO user =
  UserMemberDTO
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , gravatarHash = createGravatarHash $ user.email
    , imageUrl = user.imageUrl
    }

toGroupMemberDTO :: Group -> MemberDTO
toGroupMemberDTO group = GroupMemberDTO {gId = group.gId, name = group.name}

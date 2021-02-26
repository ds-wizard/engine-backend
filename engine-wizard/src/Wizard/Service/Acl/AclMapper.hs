module Wizard.Service.Acl.AclMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Util.Gravatar
import Wizard.Api.Resource.Acl.MemberDTO
import Wizard.Model.Acl.Acl
import Wizard.Model.User.User

toUserMemberDTO :: User -> MemberDTO
toUserMemberDTO user =
  UserMemberDTO
    { _userMemberDTOUuid = user ^. uuid
    , _userMemberDTOFirstName = user ^. firstName
    , _userMemberDTOLastName = user ^. lastName
    , _userMemberDTOGravatarHash = createGravatarHash $ user ^. email
    , _userMemberDTOImageUrl = user ^. imageUrl
    }

toGroupMemberDTO :: Group -> MemberDTO
toGroupMemberDTO group = GroupMemberDTO {_groupMemberDTOGId = group ^. gId, _groupMemberDTOName = group ^. name}

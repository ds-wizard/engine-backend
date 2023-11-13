module WizardLib.Public.Service.User.Group.UserGroupMapper where

import WizardLib.Public.Api.Resource.User.Group.UserGroupDetailDTO
import WizardLib.Public.Model.User.UserGroup
import WizardLib.Public.Model.User.UserWithMembership
import WizardLib.Public.Service.User.UserMapper

toDetailDTO :: UserGroup -> [UserWithMembership] -> UserGroupDetailDTO
toDetailDTO userGroup users =
  UserGroupDetailDTO
    { uuid = userGroup.uuid
    , name = userGroup.name
    , description = userGroup.description
    , private = userGroup.private
    , users = fmap toWithMembershipDTO users
    , createdAt = userGroup.createdAt
    , updatedAt = userGroup.updatedAt
    }

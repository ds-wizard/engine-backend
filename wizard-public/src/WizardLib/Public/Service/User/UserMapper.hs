module WizardLib.Public.Service.User.UserMapper where

import Shared.Common.Util.Gravatar (createGravatarHash)
import WizardLib.Public.Api.Resource.User.UserWithMembershipDTO
import WizardLib.Public.Model.User.UserWithMembership

toWithMembershipDTO :: UserWithMembership -> UserWithMembershipDTO
toWithMembershipDTO user =
  UserWithMembershipDTO
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , gravatarHash = createGravatarHash $ user.email
    , imageUrl = user.imageUrl
    , membershipType = user.membershipType
    }

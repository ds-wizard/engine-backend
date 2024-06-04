module WizardLib.Public.Database.Migration.Development.User.Data.Users where

import Shared.Common.Util.Uuid
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO
import WizardLib.Public.Api.Resource.User.UserWithMembershipDTO
import WizardLib.Public.Model.User.UserGroupMembership

userAlbertWithMembership :: UserWithMembershipDTO
userAlbertWithMembership =
  UserWithMembershipDTO
    { uuid = u' "7751d775-1d5e-4a43-9dc8-e43cd76f0884"
    , firstName = "Albert"
    , lastName = "Einstein"
    , gravatarHash = ".."
    , imageUrl = Nothing
    , membershipType = OwnerUserGroupMembershipType
    }

userAlbertSuggestion :: UserSuggestionDTO
userAlbertSuggestion =
  UserSuggestionDTO
    { uuid = u' "7751d775-1d5e-4a43-9dc8-e43cd76f0884"
    , firstName = "Albert"
    , lastName = "Einstein"
    , gravatarHash = ".."
    , imageUrl = Nothing
    }

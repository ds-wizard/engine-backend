module WizardLib.Public.Database.Migration.Development.User.Data.Users where

import Shared.Common.Util.Uuid
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales
import Shared.Locale.Model.Locale.Locale
import WizardLib.Public.Api.Resource.User.UserLocaleDTO
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

userLocaleEmpty :: UserLocaleDTO
userLocaleEmpty = UserLocaleDTO {lId = Nothing}

userLocale :: UserLocaleDTO
userLocale = UserLocaleDTO {lId = Just localeNl.lId}

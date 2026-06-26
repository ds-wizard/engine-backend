module Wizard.Service.User.UserMapper where

import qualified Data.Aeson as A
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Gravatar (createGravatarHash)
import Wizard.Api.Resource.Tenant.TenantCreateDTO
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.User.OnlineUserInfo
import Wizard.Model.User.User
import Wizard.Model.User.UserProfile
import WizardLib.Public.Model.PersistentCommand.User.CreateOrUpdateUserCommand
import WizardLib.Public.Model.User.UserGroupMembership
import WizardLib.Public.Model.User.UserSimple
import WizardLib.Public.Model.User.UserSuggestion
import WizardLib.Public.Model.User.UserWithMembership

toDTO :: User -> UserDTO
toDTO user =
  UserDTO
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , email = user.email
    , affiliation = user.affiliation
    , role = user.role
    , active = user.active
    , imageUrl = user.imageUrl
    , locale = user.locale
    , lastSeenNewsId = user.lastSeenNewsId
    , createdAt = user.createdAt
    , updatedAt = user.updatedAt
    , emailVerifiedAt = user.emailVerifiedAt
    , emailPending = user.emailPending
    }

toUserProfile :: UserDTO -> [U.UUID] -> M.Map U.UUID A.Value -> UserProfile
toUserProfile user userGroupUuids pluginSettings =
  UserProfile
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , email = user.email
    , imageUrl = user.imageUrl
    , role = user.role
    , lastSeenNewsId = user.lastSeenNewsId
    , userGroupUuids = userGroupUuids
    , pluginSettings = pluginSettings
    , emailVerifiedAt = user.emailVerifiedAt
    , emailPending = user.emailPending
    }

toSimple :: User -> UserSimple
toSimple user =
  UserSimple
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , email = user.email
    , imageUrl = user.imageUrl
    }

toWithMembership :: User -> UserGroupMembershipType -> UserWithMembership
toWithMembership user membershipType =
  UserWithMembership
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , email = user.email
    , imageUrl = user.imageUrl
    , membershipType = membershipType
    }

toSuggestion :: UserSimple -> UserSuggestion
toSuggestion user =
  UserSuggestion
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , gravatarHash = createGravatarHash user.email
    , imageUrl = user.imageUrl
    }

toSuggestion' :: UserDTO -> UserSuggestion
toSuggestion' user =
  UserSuggestion
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , gravatarHash = createGravatarHash user.email
    , imageUrl = user.imageUrl
    }

toOnlineUserInfo :: Maybe UserDTO -> Int -> Int -> [U.UUID] -> OnlineUserInfo
toOnlineUserInfo mUser avatarNumber colorNumber userGroupUuids =
  case mUser of
    Just user -> toLoggedOnlineUserInfo user colorNumber userGroupUuids
    Nothing -> toAnonymousOnlineUserInfo avatarNumber colorNumber

toLoggedOnlineUserInfo :: UserDTO -> Int -> [U.UUID] -> OnlineUserInfo
toLoggedOnlineUserInfo user colorNumber groupUuids =
  LoggedOnlineUserInfo
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , gravatarHash = createGravatarHash user.email
    , imageUrl = user.imageUrl
    , colorNumber = colorNumber
    , role = user.role
    , groupUuids = groupUuids
    }

toAnonymousOnlineUserInfo :: Int -> Int -> OnlineUserInfo
toAnonymousOnlineUserInfo avatarNumber colorNumber =
  AnonymousOnlineUserInfo
    { avatarNumber = avatarNumber
    , colorNumber = colorNumber
    }

fromUserCreateDTO :: UserCreateDTO -> U.UUID -> String -> U.UUID -> [String] -> String -> U.UUID -> UTCTime -> Bool -> User
fromUserCreateDTO dto userUuid passwordHash roleUuid permissions roleName tenantUuid now shouldSendRegistrationEmail =
  let active = not shouldSendRegistrationEmail
   in User
        { uuid = userUuid
        , firstName = dto.firstName
        , lastName = dto.lastName
        , email = toLower <$> dto.email
        , passwordHash = passwordHash
        , affiliation = dto.affiliation
        , role = RoleSimple {uuid = roleUuid, name = roleName, permissions = permissions}
        , active = active
        , imageUrl = Nothing
        , locale = Nothing
        , machine = False
        , lastSeenNewsId = Nothing
        , tenantUuid = tenantUuid
        , lastVisitedAt = now
        , createdAt = now
        , updatedAt = now
        , emailVerifiedAt = if active then Just now else Nothing
        , emailPending = if active then Nothing else Just (toLower <$> dto.email)
        }

fromUserExternalDTO
  :: U.UUID
  -> String
  -> String
  -> String
  -> String
  -> U.UUID
  -> [String]
  -> String
  -> Bool
  -> Maybe String
  -> U.UUID
  -> UTCTime
  -> User
fromUserExternalDTO userUuid firstName lastName email passwordHash roleUuid permissions roleName active mImageUrl tenantUuid now =
  User
    { uuid = userUuid
    , firstName = firstName
    , lastName = lastName
    , email = email
    , passwordHash = passwordHash
    , affiliation = Nothing
    , role = RoleSimple {uuid = roleUuid, name = roleName, permissions = permissions}
    , active = active
    , imageUrl = mImageUrl
    , locale = Nothing
    , machine = False
    , lastSeenNewsId = Nothing
    , tenantUuid = tenantUuid
    , lastVisitedAt = now
    , createdAt = now
    , updatedAt = now
    , emailVerifiedAt = if active then Just now else Nothing
    , emailPending = if active then Nothing else Just email
    }

fromUserChangeDTO :: UserChangeDTO -> User -> [String] -> String -> User
fromUserChangeDTO dto oldUser permissions roleName =
  User
    { uuid = oldUser.uuid
    , firstName = dto.firstName
    , lastName = dto.lastName
    , email = toLower <$> dto.email
    , passwordHash = oldUser.passwordHash
    , affiliation = dto.affiliation
    , role = RoleSimple {uuid = dto.roleUuid, name = roleName, permissions = permissions}
    , active = dto.active
    , imageUrl = oldUser.imageUrl
    , locale = oldUser.locale
    , machine = oldUser.machine
    , lastSeenNewsId = oldUser.lastSeenNewsId
    , tenantUuid = oldUser.tenantUuid
    , lastVisitedAt = oldUser.lastVisitedAt
    , createdAt = oldUser.createdAt
    , updatedAt = oldUser.updatedAt
    , emailVerifiedAt = oldUser.emailVerifiedAt
    , emailPending = oldUser.emailPending
    }

fromTenantCreateToUserCreateDTO :: TenantCreateDTO -> U.UUID -> UserCreateDTO
fromTenantCreateToUserCreateDTO dto adminRoleUuid =
  UserCreateDTO
    { firstName = dto.firstName
    , lastName = dto.lastName
    , email = dto.email
    , affiliation = Nothing
    , roleUuid = Just adminRoleUuid
    , password = dto.password
    }

fromCommandCreateDTO :: CreateOrUpdateUserCommand -> [String] -> String -> UTCTime -> User
fromCommandCreateDTO command permissions roleName now =
  User
    { uuid = command.uuid
    , firstName = command.firstName
    , lastName = command.lastName
    , email = command.email
    , passwordHash = "no-hash"
    , affiliation = command.affiliation
    , role = RoleSimple {uuid = command.roleUuid, name = roleName, permissions = permissions}
    , active = command.active
    , imageUrl = command.imageUrl
    , locale = Nothing
    , machine = False
    , lastSeenNewsId = Nothing
    , tenantUuid = command.tenantUuid
    , lastVisitedAt = now
    , createdAt = now
    , updatedAt = now
    , emailVerifiedAt = Just now
    , emailPending = Nothing
    }

fromCommandChangeDTO :: User -> CreateOrUpdateUserCommand -> [String] -> String -> UTCTime -> User
fromCommandChangeDTO oldUser command permissions roleName now =
  User
    { uuid = command.uuid
    , firstName = command.firstName
    , lastName = command.lastName
    , email = command.email
    , passwordHash = oldUser.passwordHash
    , affiliation = command.affiliation
    , role = RoleSimple {uuid = command.roleUuid, name = roleName, permissions = permissions}
    , active = command.active
    , imageUrl = command.imageUrl
    , locale = oldUser.locale
    , machine = oldUser.machine
    , lastSeenNewsId = oldUser.lastSeenNewsId
    , tenantUuid = oldUser.tenantUuid
    , lastVisitedAt = oldUser.lastVisitedAt
    , createdAt = oldUser.createdAt
    , updatedAt = now
    , emailVerifiedAt = oldUser.emailVerifiedAt
    , emailPending = oldUser.emailPending
    }

module Wizard.Service.User.UserMapper where

import Data.Char (toLower)
import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Gravatar (createGravatarHash)
import Wizard.Api.Resource.Tenant.TenantCreateDTO
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Model.User.OnlineUserInfo
import Wizard.Model.User.User
import Wizard.Model.User.UserProfile
import Wizard.Model.User.UserSuggestion
import WizardLib.Public.Model.PersistentCommand.User.CreateOrUpdateUserCommand

toDTO :: User -> UserDTO
toDTO user =
  UserDTO
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , email = user.email
    , affiliation = user.affiliation
    , sources = user.sources
    , uRole = user.uRole
    , permissions = user.permissions
    , active = user.active
    , imageUrl = user.imageUrl
    , createdAt = user.createdAt
    , updatedAt = user.updatedAt
    }

toUserProfile :: UserDTO -> [U.UUID] -> UserProfile
toUserProfile user userGroupUuids =
  UserProfile
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , email = user.email
    , imageUrl = user.imageUrl
    , uRole = user.uRole
    , permissions = user.permissions
    , userGroupUuids = userGroupUuids
    }

toSuggestion :: User -> UserSuggestion
toSuggestion user =
  UserSuggestion
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , email = user.email
    , imageUrl = user.imageUrl
    }

toSuggestionDTO :: UserSuggestion -> UserSuggestionDTO
toSuggestionDTO user =
  UserSuggestionDTO
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , gravatarHash = createGravatarHash $ user.email
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
    , gravatarHash = createGravatarHash $ user.email
    , imageUrl = user.imageUrl
    , colorNumber = colorNumber
    , role = user.uRole
    , groupUuids = groupUuids
    }

toAnonymousOnlineUserInfo :: Int -> Int -> OnlineUserInfo
toAnonymousOnlineUserInfo avatarNumber colorNumber =
  AnonymousOnlineUserInfo
    { avatarNumber = avatarNumber
    , colorNumber = colorNumber
    }

fromUserCreateDTO :: UserCreateDTO -> U.UUID -> String -> String -> [String] -> U.UUID -> UTCTime -> Bool -> User
fromUserCreateDTO dto userUuid passwordHash role permissions tenantUuid now shouldSendRegistrationEmail =
  User
    { uuid = userUuid
    , firstName = dto.firstName
    , lastName = dto.lastName
    , email = toLower <$> dto.email
    , passwordHash = passwordHash
    , affiliation = dto.affiliation
    , sources = [_USER_SOURCE_INTERNAL]
    , uRole = role
    , permissions = permissions
    , active = not shouldSendRegistrationEmail
    , submissionProps = []
    , imageUrl = Nothing
    , machine = False
    , tenantUuid = tenantUuid
    , lastVisitedAt = now
    , createdAt = now
    , updatedAt = now
    }

fromUserExternalDTO
  :: U.UUID
  -> String
  -> String
  -> String
  -> String
  -> [String]
  -> String
  -> [String]
  -> Bool
  -> Maybe String
  -> U.UUID
  -> UTCTime
  -> User
fromUserExternalDTO userUuid firstName lastName email passwordHash sources uRole permissions active mImageUrl tenantUuid now =
  User
    { uuid = userUuid
    , firstName = firstName
    , lastName = lastName
    , email = email
    , passwordHash = passwordHash
    , affiliation = Nothing
    , sources = sources
    , uRole = uRole
    , permissions = permissions
    , active = active
    , submissionProps = []
    , imageUrl = mImageUrl
    , machine = False
    , tenantUuid = tenantUuid
    , lastVisitedAt = now
    , createdAt = now
    , updatedAt = now
    }

fromUpdateUserExternalDTO :: User -> String -> String -> Maybe String -> String -> UTCTime -> User
fromUpdateUserExternalDTO oldUser firstName lastName mImageUrl serviceId now =
  User
    { uuid = oldUser.uuid
    , firstName = firstName
    , lastName = lastName
    , email = oldUser.email
    , passwordHash = oldUser.passwordHash
    , affiliation = oldUser.affiliation
    , sources =
        case L.find (== serviceId) oldUser.sources of
          Just _ -> oldUser.sources
          Nothing -> oldUser.sources ++ [serviceId]
    , uRole = oldUser.uRole
    , permissions = oldUser.permissions
    , active = oldUser.active
    , submissionProps = oldUser.submissionProps
    , imageUrl = mImageUrl
    , machine = oldUser.machine
    , tenantUuid = oldUser.tenantUuid
    , lastVisitedAt = now
    , createdAt = oldUser.createdAt
    , updatedAt = oldUser.updatedAt
    }

fromUserChangeDTO :: UserChangeDTO -> User -> [String] -> User
fromUserChangeDTO dto oldUser permission =
  User
    { uuid = oldUser.uuid
    , firstName = dto.firstName
    , lastName = dto.lastName
    , email = toLower <$> dto.email
    , passwordHash = oldUser.passwordHash
    , affiliation = dto.affiliation
    , sources = oldUser.sources
    , uRole = dto.uRole
    , permissions = permission
    , active = dto.active
    , submissionProps = oldUser.submissionProps
    , imageUrl = oldUser.imageUrl
    , machine = oldUser.machine
    , tenantUuid = oldUser.tenantUuid
    , lastVisitedAt = oldUser.lastVisitedAt
    , createdAt = oldUser.createdAt
    , updatedAt = oldUser.updatedAt
    }

fromTenantCreateToUserCreateDTO :: TenantCreateDTO -> UserCreateDTO
fromTenantCreateToUserCreateDTO dto =
  UserCreateDTO
    { firstName = dto.firstName
    , lastName = dto.lastName
    , email = dto.email
    , affiliation = Nothing
    , uRole = Just _USER_ROLE_ADMIN
    , password = dto.password
    }

fromCommandCreateDTO :: CreateOrUpdateUserCommand -> [String] -> UTCTime -> User
fromCommandCreateDTO command permissions now =
  User
    { uuid = command.uuid
    , firstName = command.firstName
    , lastName = command.lastName
    , email = command.email
    , passwordHash = "no-hash"
    , affiliation = command.affiliation
    , sources = command.sources
    , uRole = command.uRole
    , permissions = permissions
    , active = command.active
    , submissionProps = []
    , imageUrl = command.imageUrl
    , machine = False
    , tenantUuid = command.tenantUuid
    , lastVisitedAt = now
    , createdAt = now
    , updatedAt = now
    }

fromCommandChangeDTO :: User -> CreateOrUpdateUserCommand -> [String] -> UTCTime -> User
fromCommandChangeDTO oldUser command permissions now =
  User
    { uuid = command.uuid
    , firstName = command.firstName
    , lastName = command.lastName
    , email = command.email
    , passwordHash = oldUser.passwordHash
    , affiliation = command.affiliation
    , sources = command.sources
    , uRole = command.uRole
    , permissions = permissions
    , active = command.active
    , submissionProps = oldUser.submissionProps
    , imageUrl = command.imageUrl
    , machine = oldUser.machine
    , tenantUuid = oldUser.tenantUuid
    , lastVisitedAt = oldUser.lastVisitedAt
    , createdAt = oldUser.createdAt
    , updatedAt = now
    }

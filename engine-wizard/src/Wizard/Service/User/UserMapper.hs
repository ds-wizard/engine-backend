module Wizard.Service.User.UserMapper where

import Data.Char (toLower)
import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import Shared.Util.Gravatar (createGravatarHash)
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Model.User.OnlineUserInfo
import Wizard.Model.User.User
import Wizard.Model.User.UserSuggestion

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
    , groups = user.groups
    , createdAt = user.createdAt
    , updatedAt = user.updatedAt
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

toOnlineUserInfo :: Maybe UserDTO -> Int -> Int -> OnlineUserInfo
toOnlineUserInfo mUser avatarNumber colorNumber =
  case mUser of
    Just user -> toLoggedOnlineUserInfo user colorNumber
    Nothing -> toAnonymousOnlineUserInfo avatarNumber colorNumber

toLoggedOnlineUserInfo :: UserDTO -> Int -> OnlineUserInfo
toLoggedOnlineUserInfo user colorNumber =
  LoggedOnlineUserInfo
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , gravatarHash = createGravatarHash $ user.email
    , imageUrl = user.imageUrl
    , colorNumber = colorNumber
    , role = user.uRole
    , groups = user.groups
    }

toAnonymousOnlineUserInfo :: Int -> Int -> OnlineUserInfo
toAnonymousOnlineUserInfo avatarNumber colorNumber =
  AnonymousOnlineUserInfo
    { avatarNumber = avatarNumber
    , colorNumber = colorNumber
    }

fromUserCreateDTO :: UserCreateDTO -> U.UUID -> String -> String -> [String] -> U.UUID -> UTCTime -> Bool -> User
fromUserCreateDTO dto userUuid passwordHash role permissions appUuid now shouldSendRegistrationEmail =
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
    , groups = []
    , machine = False
    , appUuid = appUuid
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
  -> Maybe String
  -> U.UUID
  -> UTCTime
  -> User
fromUserExternalDTO userUuid firstName lastName email passwordHash sources uRole permissions mImageUrl appUuid now =
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
    , active = True
    , submissionProps = []
    , imageUrl = mImageUrl
    , groups = []
    , machine = False
    , appUuid = appUuid
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
    , groups = oldUser.groups
    , machine = oldUser.machine
    , appUuid = oldUser.appUuid
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
    , groups = oldUser.groups
    , machine = oldUser.machine
    , appUuid = oldUser.appUuid
    , lastVisitedAt = oldUser.lastVisitedAt
    , createdAt = oldUser.createdAt
    , updatedAt = oldUser.updatedAt
    }

fromAppCreateToUserCreateDTO :: AppCreateDTO -> UserCreateDTO
fromAppCreateToUserCreateDTO dto =
  UserCreateDTO
    { firstName = dto.firstName
    , lastName = dto.lastName
    , email = dto.email
    , affiliation = Nothing
    , uRole = Just _USER_ROLE_ADMIN
    , password = dto.password
    }

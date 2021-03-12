module Wizard.Service.User.UserMapper where

import Control.Lens ((^.))
import Data.Char (toLower)
import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Util.Gravatar (createGravatarHash)
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
    { _userDTOUuid = user ^. uuid
    , _userDTOFirstName = user ^. firstName
    , _userDTOLastName = user ^. lastName
    , _userDTOEmail = user ^. email
    , _userDTOAffiliation = user ^. affiliation
    , _userDTOSources = user ^. sources
    , _userDTORole = user ^. role
    , _userDTOPermissions = user ^. permissions
    , _userDTOActive = user ^. active
    , _userDTOImageUrl = user ^. imageUrl
    , _userDTOGroups = user ^. groups
    , _userDTOCreatedAt = user ^. createdAt
    , _userDTOUpdatedAt = user ^. updatedAt
    }

toSuggestion :: User -> UserSuggestion
toSuggestion user =
  UserSuggestion
    { _userSuggestionUuid = user ^. uuid
    , _userSuggestionFirstName = user ^. firstName
    , _userSuggestionLastName = user ^. lastName
    , _userSuggestionEmail = user ^. email
    , _userSuggestionImageUrl = user ^. imageUrl
    }

toSuggestionDTO :: UserSuggestion -> UserSuggestionDTO
toSuggestionDTO user =
  UserSuggestionDTO
    { _userSuggestionDTOUuid = user ^. uuid
    , _userSuggestionDTOFirstName = user ^. firstName
    , _userSuggestionDTOLastName = user ^. lastName
    , _userSuggestionDTOGravatarHash = createGravatarHash $ user ^. email
    , _userSuggestionDTOImageUrl = user ^. imageUrl
    }

toOnlineUserInfo :: Maybe UserDTO -> Int -> Int -> OnlineUserInfo
toOnlineUserInfo mUser avatarNumber colorNumber =
  case mUser of
    Just user -> toLoggedOnlineUserInfo user colorNumber
    Nothing -> toAnonymousOnlineUserInfo avatarNumber colorNumber

toLoggedOnlineUserInfo :: UserDTO -> Int -> OnlineUserInfo
toLoggedOnlineUserInfo user colorNumber =
  LoggedOnlineUserInfo
    { _loggedOnlineUserInfoUuid = user ^. uuid
    , _loggedOnlineUserInfoFirstName = user ^. firstName
    , _loggedOnlineUserInfoLastName = user ^. lastName
    , _loggedOnlineUserInfoGravatarHash = createGravatarHash $ user ^. email
    , _loggedOnlineUserInfoImageUrl = user ^. imageUrl
    , _loggedOnlineUserInfoColorNumber = colorNumber
    , _loggedOnlineUserInfoRole = user ^. role
    , _loggedOnlineUserInfoGroups = user ^. groups
    }

toAnonymousOnlineUserInfo :: Int -> Int -> OnlineUserInfo
toAnonymousOnlineUserInfo avatarNumber colorNumber =
  AnonymousOnlineUserInfo
    {_anonymousOnlineUserInfoAvatarNumber = avatarNumber, _anonymousOnlineUserInfoColorNumber = colorNumber}

fromUserCreateDTO :: UserCreateDTO -> U.UUID -> String -> String -> [String] -> UTCTime -> User
fromUserCreateDTO dto userUuid passwordHash role permissions now =
  User
    { _userUuid = userUuid
    , _userFirstName = dto ^. firstName
    , _userLastName = dto ^. lastName
    , _userEmail = toLower <$> dto ^. email
    , _userPasswordHash = passwordHash
    , _userAffiliation = dto ^. affiliation
    , _userSources = [_USER_SOURCE_INTERNAL]
    , _userRole = role
    , _userPermissions = permissions
    , _userActive = False
    , _userSubmissionProps = []
    , _userImageUrl = Nothing
    , _userGroups = []
    , _userLastVisitedAt = now
    , _userCreatedAt = Just now
    , _userUpdatedAt = Just now
    }

fromUserExternalDTO ::
     U.UUID -> String -> String -> String -> String -> [String] -> String -> [String] -> Maybe String -> UTCTime -> User
fromUserExternalDTO userUuid firstName lastName email passwordHash sources role permissions mImageUrl now =
  User
    { _userUuid = userUuid
    , _userFirstName = firstName
    , _userLastName = lastName
    , _userEmail = email
    , _userPasswordHash = passwordHash
    , _userAffiliation = Nothing
    , _userSources = sources
    , _userRole = role
    , _userPermissions = permissions
    , _userActive = True
    , _userSubmissionProps = []
    , _userImageUrl = mImageUrl
    , _userGroups = []
    , _userLastVisitedAt = now
    , _userCreatedAt = Just now
    , _userUpdatedAt = Just now
    }

fromUpdateUserExternalDTO :: User -> String -> String -> Maybe String -> String -> UTCTime -> User
fromUpdateUserExternalDTO oldUser firstName lastName mImageUrl serviceId now =
  User
    { _userUuid = oldUser ^. uuid
    , _userFirstName = firstName
    , _userLastName = lastName
    , _userEmail = oldUser ^. email
    , _userPasswordHash = oldUser ^. passwordHash
    , _userAffiliation = oldUser ^. affiliation
    , _userSources =
        case L.find (== serviceId) (oldUser ^. sources) of
          Just _ -> oldUser ^. sources
          Nothing -> (oldUser ^. sources) ++ [serviceId]
    , _userRole = oldUser ^. role
    , _userPermissions = oldUser ^. permissions
    , _userActive = oldUser ^. active
    , _userSubmissionProps = oldUser ^. submissionProps
    , _userImageUrl = mImageUrl
    , _userGroups = oldUser ^. groups
    , _userLastVisitedAt = now
    , _userCreatedAt = oldUser ^. createdAt
    , _userUpdatedAt = oldUser ^. updatedAt
    }

fromUserChangeDTO :: UserChangeDTO -> User -> [String] -> User
fromUserChangeDTO dto oldUser permission =
  User
    { _userUuid = oldUser ^. uuid
    , _userFirstName = dto ^. firstName
    , _userLastName = dto ^. lastName
    , _userEmail = toLower <$> dto ^. email
    , _userPasswordHash = oldUser ^. passwordHash
    , _userAffiliation = dto ^. affiliation
    , _userSources = oldUser ^. sources
    , _userRole = dto ^. role
    , _userPermissions = permission
    , _userActive = dto ^. active
    , _userSubmissionProps = oldUser ^. submissionProps
    , _userImageUrl = oldUser ^. imageUrl
    , _userGroups = oldUser ^. groups
    , _userLastVisitedAt = oldUser ^. lastVisitedAt
    , _userCreatedAt = oldUser ^. createdAt
    , _userUpdatedAt = oldUser ^. updatedAt
    }

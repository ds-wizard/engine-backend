module Wizard.Service.User.UserMapper where

import Control.Lens ((^.))
import Data.Char (toLower)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Model.User.User

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
    , _userDTOCreatedAt = user ^. createdAt
    , _userDTOUpdatedAt = user ^. updatedAt
    }

fromUserCreateDTO :: UserCreateDTO -> U.UUID -> String -> Role -> [Permission] -> UTCTime -> UTCTime -> User
fromUserCreateDTO dto userUuid passwordHash role permissions createdAt updatedAt =
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
    , _userCreatedAt = Just createdAt
    , _userUpdatedAt = Just updatedAt
    }

fromUserExternalDTO ::
     U.UUID -> String -> String -> String -> String -> [String] -> Role -> [Permission] -> UTCTime -> User
fromUserExternalDTO userUuid firstName lastName email passwordHash sources role permissions now =
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
    , _userCreatedAt = Just now
    , _userUpdatedAt = Just now
    }

fromUserChangeDTO :: UserChangeDTO -> User -> [Permission] -> User
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
    , _userCreatedAt = oldUser ^. createdAt
    , _userUpdatedAt = oldUser ^. updatedAt
    }

fromUserProfileChangeDTO :: UserProfileChangeDTO -> User -> User
fromUserProfileChangeDTO dto oldUser =
  User
    { _userUuid = oldUser ^. uuid
    , _userFirstName = dto ^. firstName
    , _userLastName = dto ^. lastName
    , _userEmail = toLower <$> dto ^. email
    , _userPasswordHash = oldUser ^. passwordHash
    , _userAffiliation = dto ^. affiliation
    , _userSources = oldUser ^. sources
    , _userRole = oldUser ^. role
    , _userPermissions = oldUser ^. permissions
    , _userActive = oldUser ^. active
    , _userCreatedAt = oldUser ^. createdAt
    , _userUpdatedAt = oldUser ^. updatedAt
    }

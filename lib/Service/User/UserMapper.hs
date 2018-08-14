module Service.User.UserMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import Api.Resource.User.UserChangeDTO
import Api.Resource.User.UserCreateDTO
import Api.Resource.User.UserDTO
import Api.Resource.User.UserProfileChangeDTO
import LensesConfig
import Model.User.User

toDTO :: User -> UserDTO
toDTO user =
  UserDTO
  { _userDTOUuid = user ^. uuid
  , _userDTOName = user ^. name
  , _userDTOSurname = user ^. surname
  , _userDTOEmail = user ^. email
  , _userDTORole = user ^. role
  , _userDTOPermissions = user ^. permissions
  , _userDTOIsActive = user ^. isActive
  , _userDTOCreatedAt = user ^. createdAt
  , _userDTOUpdatedAt = user ^. updatedAt
  }

fromUserCreateDTO :: UserCreateDTO -> U.UUID -> String -> Role -> [Permission] -> UTCTime -> UTCTime -> User
fromUserCreateDTO dto userUuid passwordHash role permissions createdAt updatedAt =
  User
  { _userUuid = userUuid
  , _userName = dto ^. name
  , _userSurname = dto ^. surname
  , _userEmail = dto ^. email
  , _userPasswordHash = passwordHash
  , _userRole = role
  , _userPermissions = permissions
  , _userIsActive = False
  , _userCreatedAt = Just createdAt
  , _userUpdatedAt = Just updatedAt
  }

fromUserChangeDTO :: UserChangeDTO -> User -> [Permission] -> User
fromUserChangeDTO dto oldUser permission =
  User
  { _userUuid = oldUser ^. uuid
  , _userName = dto ^. name
  , _userSurname = dto ^. surname
  , _userEmail = dto ^. email
  , _userPasswordHash = oldUser ^. passwordHash
  , _userRole = dto ^. role
  , _userPermissions = permission
  , _userIsActive = dto ^. isActive
  , _userCreatedAt = oldUser ^. createdAt
  , _userUpdatedAt = oldUser ^. updatedAt
  }

fromUserProfileChangeDTO :: UserProfileChangeDTO -> User -> User
fromUserProfileChangeDTO dto oldUser =
  User
  { _userUuid = oldUser ^. uuid
  , _userName = dto ^. name
  , _userSurname = dto ^. surname
  , _userEmail = dto ^. email
  , _userPasswordHash = oldUser ^. passwordHash
  , _userRole = oldUser ^. role
  , _userPermissions = oldUser ^. permissions
  , _userIsActive = oldUser ^. isActive
  , _userCreatedAt = oldUser ^. createdAt
  , _userUpdatedAt = oldUser ^. updatedAt
  }

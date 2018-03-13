module Service.User.UserMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resource.User.UserCreateDTO
import Api.Resource.User.UserDTO
import Common.Types
import Model.User.User

toDTO :: User -> UserDTO
toDTO user =
  UserDTO
  { _udtoUuid = user ^. uUuid
  , _udtoName = user ^. uName
  , _udtoSurname = user ^. uSurname
  , _udtoEmail = user ^. uEmail
  , _udtoRole = user ^. uRole
  , _udtoPermissions = user ^. uPermissions
  , _udtoIsActive = user ^. uIsActive
  }

fromUserCreateDTO :: UserCreateDTO -> UUID -> String -> Role -> [Permission] -> Bool -> User
fromUserCreateDTO dto userUuid passwordHash role permissions isActive =
  User
  { _uUuid = userUuid
  , _uName = dto ^. ucdtoName
  , _uSurname = dto ^. ucdtoSurname
  , _uEmail = dto ^. ucdtoEmail
  , _uPasswordHash = passwordHash
  , _uRole = role
  , _uPermissions = permissions
  , _uIsActive = isActive
  }

fromUserDTO :: UserDTO -> UUID -> String -> Bool -> User
fromUserDTO dto userUuid passwordHash isActive =
  User
  { _uUuid = userUuid
  , _uName = dto ^. udtoName
  , _uSurname = dto ^. udtoSurname
  , _uEmail = dto ^. udtoEmail
  , _uPasswordHash = passwordHash
  , _uRole = dto ^. udtoRole
  , _uPermissions = dto ^. udtoPermissions
  , _uIsActive = isActive
  }

module Service.User.UserMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resources.User.UserCreateDTO
import Api.Resources.User.UserDTO
import Common.Types
import Database.Entity.User

toDTO :: User -> UserDTO
toDTO user =
  UserDTO
  { _udtoUuid = user ^. uUuid
  , _udtoName = user ^. uName
  , _udtoSurname = user ^. uSurname
  , _udtoEmail = user ^. uEmail
  , _udtoRole = user ^. uRole
  , _udtoPermissions = user ^. uPermissions
  }

fromUserCreateDTO :: UserCreateDTO -> UUID -> String -> [Permission] -> User
fromUserCreateDTO dto userUuid passwordHash permissions =
  User
  { _uUuid = userUuid
  , _uName = dto ^. ucdtoName
  , _uSurname = dto ^. ucdtoSurname
  , _uEmail = dto ^. ucdtoEmail
  , _uPasswordHash = passwordHash
  , _uRole = dto ^. ucdtoRole
  , _uPermissions = permissions
  }

fromUserDTO :: UserDTO -> UUID -> String -> User
fromUserDTO dto userUuid passwordHash =
  User
  { _uUuid = userUuid
  , _uName = dto ^. udtoName
  , _uSurname = dto ^. udtoSurname
  , _uEmail = dto ^. udtoEmail
  , _uPasswordHash = passwordHash
  , _uRole = dto ^. udtoRole
  , _uPermissions = dto ^. udtoPermissions
  }
    
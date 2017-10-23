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
  , _udtoRole = user ^. uRole
  , _udtoPermissions = user ^. uPermissions
  }

fromUserCreateDTO :: UserCreateDTO -> UUID -> [Permission] -> User
fromUserCreateDTO dto uuid permissions =
  User
  { _uUuid = uuid
  , _uName = dto ^. ucdtoName
  , _uSurname = dto ^. ucdtoSurname
  , _uRole = dto ^. ucdtoRole
  , _uPermissions = permissions
  }

fromUserDTO :: UserDTO -> User
fromUserDTO dto =
  User
  { _uUuid = dto ^. udtoUuid
  , _uName = dto ^. udtoName
  , _uSurname = dto ^. udtoSurname
  , _uRole = dto ^. udtoRole
  , _uPermissions = dto ^. udtoPermissions
  }
    
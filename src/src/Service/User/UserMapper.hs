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

fromDTO :: UserCreateDTO -> UUID -> [Permission] -> User
fromDTO dto uuid permissions =
  User
  { _uUuid = uuid
  , _uName = dto ^. ucdtoName
  , _uSurname = dto ^. ucdtoSurname
  , _uRole = dto ^. ucdtoRole
  , _uPermissions = permissions
  }

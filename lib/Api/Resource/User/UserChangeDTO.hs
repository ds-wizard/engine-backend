module Api.Resource.User.UserChangeDTO where

import Control.Monad
import Data.Aeson
import Data.UUID

import Model.User.User

data UserChangeDTO = UserChangeDTO
  { _userChangeDTOUuid :: UUID
  , _userChangeDTOName :: String
  , _userChangeDTOSurname :: String
  , _userChangeDTOEmail :: Email
  , _userChangeDTORole :: Role
  , _userChangeDTOActive :: Bool
  }

instance FromJSON UserChangeDTO where
  parseJSON (Object o) = do
    _userChangeDTOUuid <- o .: "uuid"
    _userChangeDTOName <- o .: "name"
    _userChangeDTOSurname <- o .: "surname"
    _userChangeDTOEmail <- o .: "email"
    _userChangeDTORole <- o .: "role"
    _userChangeDTOActive <- o .: "active"
    return UserChangeDTO {..}
  parseJSON _ = mzero

instance ToJSON UserChangeDTO where
  toJSON UserChangeDTO {..} =
    object
      [ "uuid" .= _userChangeDTOUuid
      , "name" .= _userChangeDTOName
      , "surname" .= _userChangeDTOSurname
      , "email" .= _userChangeDTOEmail
      , "role" .= _userChangeDTORole
      , "active" .= _userChangeDTOActive
      ]

module Api.Resource.User.UserChangeDTO where

import Control.Monad
import Data.Aeson
import Data.UUID

import Common.Types

data UserChangeDTO = UserChangeDTO
  { _userChangeDTOUuid :: UUID
  , _userChangeDTOName :: String
  , _userChangeDTOSurname :: String
  , _userChangeDTOEmail :: Email
  , _userChangeDTORole :: Role
  , _userChangeDTOIsActive :: Bool
  }

instance FromJSON UserChangeDTO where
  parseJSON (Object o) = do
    _userChangeDTOUuid <- o .: "uuid"
    _userChangeDTOName <- o .: "name"
    _userChangeDTOSurname <- o .: "surname"
    _userChangeDTOEmail <- o .: "email"
    _userChangeDTORole <- o .: "role"
    _userChangeDTOIsActive <- o .: "isActive"
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
      , "isActive" .= _userChangeDTOIsActive
      ]

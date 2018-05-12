module Api.Resource.User.UserCreateDTO where

import Control.Monad
import Data.Aeson

import Common.Types

data UserCreateDTO = UserCreateDTO
  { _userCreateDTOName :: String
  , _userCreateDTOSurname :: String
  , _userCreateDTOEmail :: Email
  , _userCreateDTORole :: Maybe Role
  , _userCreateDTOPassword :: String
  }

instance FromJSON UserCreateDTO where
  parseJSON (Object o) = do
    _userCreateDTOName <- o .: "name"
    _userCreateDTOSurname <- o .: "surname"
    _userCreateDTOEmail <- o .: "email"
    _userCreateDTORole <- o .: "role"
    _userCreateDTOPassword <- o .: "password"
    return UserCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON UserCreateDTO where
  toJSON UserCreateDTO {..} =
    object
      [ "name" .= _userCreateDTOName
      , "surname" .= _userCreateDTOSurname
      , "email" .= _userCreateDTOEmail
      , "role" .= _userCreateDTORole
      , "password" .= _userCreateDTOPassword
      ]

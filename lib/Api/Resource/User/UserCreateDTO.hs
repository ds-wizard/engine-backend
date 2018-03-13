module Api.Resource.User.UserCreateDTO where

import Control.Lens ((^.), makeLenses)
import Control.Monad
import Data.Aeson
import Data.Text

import Common.Types

data UserCreateDTO = UserCreateDTO
  { _ucdtoName :: String
  , _ucdtoSurname :: String
  , _ucdtoEmail :: Email
  , _ucdtoRole :: Maybe Role
  , _ucdtoPassword :: String
  }

makeLenses ''UserCreateDTO

instance FromJSON UserCreateDTO where
  parseJSON (Object o) = do
    _ucdtoName <- o .: "name"
    _ucdtoSurname <- o .: "surname"
    _ucdtoEmail <- o .: "email"
    _ucdtoRole <- o .: "role"
    _ucdtoPassword <- o .: "password"
    return UserCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON UserCreateDTO where
  toJSON UserCreateDTO {..} =
    object
      [ "name" .= _ucdtoName
      , "surname" .= _ucdtoSurname
      , "email" .= _ucdtoEmail
      , "role" .= _ucdtoRole
      , "password" .= _ucdtoPassword
      ]

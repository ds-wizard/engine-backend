module Api.Resources.User.UserCreateDTO where

import Data.Aeson
import Control.Lens (makeLenses, (^.))
import Control.Monad
import Data.Text

import Common.Types


data UserCreateDTO = UserCreateDTO
  { _ucdtoName :: String
  , _ucdtoSurname :: String
  , _ucdtoEmail :: Email
  , _ucdtoRole :: Role
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

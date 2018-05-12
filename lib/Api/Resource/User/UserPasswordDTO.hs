module Api.Resource.User.UserPasswordDTO where

import Control.Monad
import Data.Aeson

data UserPasswordDTO = UserPasswordDTO
  { _userPasswordDTOPassword :: String
  }

instance FromJSON UserPasswordDTO where
  parseJSON (Object o) = do
    _userPasswordDTOPassword <- o .: "password"
    return UserPasswordDTO {..}
  parseJSON _ = mzero

instance ToJSON UserPasswordDTO where
  toJSON UserPasswordDTO {..} = object ["password" .= _userPasswordDTOPassword]

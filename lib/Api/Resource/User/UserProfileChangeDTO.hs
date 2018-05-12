module Api.Resource.User.UserProfileChangeDTO where

import Control.Monad
import Data.Aeson

import Common.Types

data UserProfileChangeDTO = UserProfileChangeDTO
  { _userProfileChangeDTOName :: String
  , _userProfileChangeDTOSurname :: String
  , _userProfileChangeDTOEmail :: Email
  }

instance FromJSON UserProfileChangeDTO where
  parseJSON (Object o) = do
    _userProfileChangeDTOName <- o .: "name"
    _userProfileChangeDTOSurname <- o .: "surname"
    _userProfileChangeDTOEmail <- o .: "email"
    return UserProfileChangeDTO {..}
  parseJSON _ = mzero

instance ToJSON UserProfileChangeDTO where
  toJSON UserProfileChangeDTO {..} =
    object
      [ "name" .= _userProfileChangeDTOName
      , "surname" .= _userProfileChangeDTOSurname
      , "email" .= _userProfileChangeDTOEmail
      ]

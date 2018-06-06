module Api.Resource.Token.TokenCreateDTO where

import Control.Monad
import Data.Aeson

data TokenCreateDTO = TokenCreateDTO
  { _tokenCreateDTOEmail :: String
  , _tokenCreateDTOPassword :: String
  }

instance FromJSON TokenCreateDTO where
  parseJSON (Object o) = do
    _tokenCreateDTOEmail <- o .: "email"
    _tokenCreateDTOPassword <- o .: "password"
    return TokenCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON TokenCreateDTO where
  toJSON TokenCreateDTO {..} = object ["email" .= _tokenCreateDTOEmail, "password" .= _tokenCreateDTOPassword]

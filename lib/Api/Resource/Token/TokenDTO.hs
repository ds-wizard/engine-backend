module Api.Resource.Token.TokenDTO where

import Data.Aeson

data TokenDTO = TokenDTO
  { _tokenDTOToken :: String
  }

instance ToJSON TokenDTO where
  toJSON TokenDTO {..} = object ["token" .= _tokenDTOToken]

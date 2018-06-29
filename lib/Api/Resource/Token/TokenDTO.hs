module Api.Resource.Token.TokenDTO where

import Control.Monad
import Data.Aeson

data TokenDTO = TokenDTO
  { _tokenDTOToken :: String
  } deriving (Show, Eq)

instance ToJSON TokenDTO where
  toJSON TokenDTO {..} = object ["token" .= _tokenDTOToken]

instance FromJSON TokenDTO where
  parseJSON (Object o) = do
    _tokenDTOToken <- o .: "token"
    return TokenDTO {..}
  parseJSON _ = mzero

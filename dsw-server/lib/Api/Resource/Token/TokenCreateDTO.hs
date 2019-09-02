module Api.Resource.Token.TokenCreateDTO where

import GHC.Generics

data TokenCreateDTO = TokenCreateDTO
  { _tokenCreateDTOEmail :: String
  , _tokenCreateDTOPassword :: String
  } deriving (Generic)

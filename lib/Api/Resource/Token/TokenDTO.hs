module Api.Resource.Token.TokenDTO where

import GHC.Generics

data TokenDTO = TokenDTO
  { _tokenDTOToken :: String
  } deriving (Show, Eq, Generic)

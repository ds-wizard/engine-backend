module Shared.OpenId.Api.Resource.OpenId.Client.Flow.OpenIdClientAuthenticationUrlDTO where

import GHC.Generics

data OpenIdClientAuthenticationUrlDTO = OpenIdClientAuthenticationUrlDTO
  { url :: String
  , state :: String
  }
  deriving (Generic, Eq, Show)

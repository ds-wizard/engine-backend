module Shared.OpenId.Model.OpenId.OpenIdClientParameter where

import GHC.Generics

data OpenIdClientParameter = OpenIdClientParameter
  { name :: String
  , value :: String
  }
  deriving (Generic, Eq, Show)

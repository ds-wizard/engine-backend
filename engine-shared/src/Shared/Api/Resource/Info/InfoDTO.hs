module Shared.Api.Resource.Info.InfoDTO where

import GHC.Generics

data InfoDTO = InfoDTO
  { name :: String
  , version :: String
  , builtAt :: String
  }
  deriving (Show, Eq, Generic)

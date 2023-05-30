module Shared.ActionKey.Api.Resource.ActionKey.ActionKeyDTO where

import GHC.Generics

data ActionKeyDTO aType = ActionKeyDTO
  { aType :: aType
  , email :: String
  }
  deriving (Show, Eq, Generic)

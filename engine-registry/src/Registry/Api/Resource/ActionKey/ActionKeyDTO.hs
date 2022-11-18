module Registry.Api.Resource.ActionKey.ActionKeyDTO where

import GHC.Generics

import Registry.Model.ActionKey.ActionKey

data ActionKeyDTO = ActionKeyDTO
  { aType :: ActionKeyType
  , email :: String
  }
  deriving (Show, Eq, Generic)

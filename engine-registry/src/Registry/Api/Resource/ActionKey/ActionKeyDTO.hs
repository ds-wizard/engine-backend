module Registry.Api.Resource.ActionKey.ActionKeyDTO where

import GHC.Generics

import Registry.Model.ActionKey.ActionKey

data ActionKeyDTO =
  ActionKeyDTO
    { _actionKeyDTOAType :: ActionKeyType
    , _actionKeyDTOEmail :: String
    }
  deriving (Show, Eq, Generic)

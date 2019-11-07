module Api.Resource.ActionKey.ActionKeyDTO where

import GHC.Generics

import Model.ActionKey.ActionKey

data ActionKeyDTO = ActionKeyDTO
  { _actionKeyDTOAType :: ActionKeyType
  , _actionKeyDTOEmail :: String
  } deriving (Show, Eq, Generic)

module Api.Resource.ActionKey.ActionKeyDTO where

import GHC.Generics

data ActionKeyDTO = ActionKeyDTO
  { _actionKeyDTOAType :: String
  , _actionKeyDTOEmail :: String
  } deriving (Show, Eq, Generic)

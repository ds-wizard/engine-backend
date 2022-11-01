module Wizard.Api.Resource.UserToken.UserTokenDTO where

import GHC.Generics

data UserTokenDTO =
  UserTokenDTO
    { _userTokenDTOToken :: String
    }
  deriving (Show, Eq, Generic)

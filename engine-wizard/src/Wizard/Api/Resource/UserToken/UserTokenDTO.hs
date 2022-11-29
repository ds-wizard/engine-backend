module Wizard.Api.Resource.UserToken.UserTokenDTO where

import GHC.Generics

data UserTokenDTO = UserTokenDTO
  { token :: String
  }
  deriving (Show, Eq, Generic)

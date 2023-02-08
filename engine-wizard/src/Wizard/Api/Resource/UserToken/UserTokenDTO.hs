module Wizard.Api.Resource.UserToken.UserTokenDTO where

import GHC.Generics

data UserTokenDTO
  = CodeRequiredDTO
  | UserTokenDTO
      { token :: String
      }
  deriving (Show, Eq, Generic)

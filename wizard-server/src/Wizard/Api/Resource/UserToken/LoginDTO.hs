module Wizard.Api.Resource.UserToken.LoginDTO where

import GHC.Generics

data LoginDTO = LoginDTO
  { email :: String
  , password :: String
  , code :: Maybe Int
  }
  deriving (Generic)

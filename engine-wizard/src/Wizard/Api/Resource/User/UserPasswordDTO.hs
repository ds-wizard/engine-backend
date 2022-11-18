module Wizard.Api.Resource.User.UserPasswordDTO where

import GHC.Generics

data UserPasswordDTO = UserPasswordDTO
  { password :: String
  }
  deriving (Generic)

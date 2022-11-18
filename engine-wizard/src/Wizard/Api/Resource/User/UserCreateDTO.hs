module Wizard.Api.Resource.User.UserCreateDTO where

import GHC.Generics

data UserCreateDTO = UserCreateDTO
  { firstName :: String
  , lastName :: String
  , email :: String
  , affiliation :: Maybe String
  , uRole :: Maybe String
  , password :: String
  }
  deriving (Generic)

module Wizard.Api.Resource.UserToken.UserTokenCreateDTO where

import GHC.Generics

data UserTokenCreateDTO = UserTokenCreateDTO
  { email :: String
  , password :: String
  , code :: Maybe Int
  }
  deriving (Generic)

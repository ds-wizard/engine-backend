module Wizard.Api.Resource.User.UserChangeDTO where

import GHC.Generics

data UserChangeDTO = UserChangeDTO
  { firstName :: String
  , lastName :: String
  , email :: String
  , affiliation :: Maybe String
  , uRole :: String
  , active :: Bool
  }
  deriving (Generic)

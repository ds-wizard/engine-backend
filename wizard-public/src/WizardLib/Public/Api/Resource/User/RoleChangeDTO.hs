module WizardLib.Public.Api.Resource.User.RoleChangeDTO where

import GHC.Generics

data RoleChangeDTO = RoleChangeDTO
  { name :: String
  , permissions :: [String]
  }
  deriving (Show, Eq, Generic)

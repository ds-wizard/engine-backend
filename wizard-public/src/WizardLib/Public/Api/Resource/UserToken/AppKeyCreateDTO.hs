module WizardLib.Public.Api.Resource.UserToken.AppKeyCreateDTO where

import GHC.Generics

data AppKeyCreateDTO = AppKeyCreateDTO
  { name :: String
  }
  deriving (Generic)

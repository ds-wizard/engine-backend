module WizardLib.Public.Api.Resource.User.UserFromExternalDTO where

import GHC.Generics

data UserFromExternalDTO = UserFromExternalDTO
  { hash :: String
  , email :: String
  , firstName :: String
  , lastName :: String
  }
  deriving (Generic, Show)

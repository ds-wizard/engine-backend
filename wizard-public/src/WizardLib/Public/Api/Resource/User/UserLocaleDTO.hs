module WizardLib.Public.Api.Resource.User.UserLocaleDTO where

import GHC.Generics

data UserLocaleDTO = UserLocaleDTO
  { lId :: Maybe String
  }
  deriving (Generic)

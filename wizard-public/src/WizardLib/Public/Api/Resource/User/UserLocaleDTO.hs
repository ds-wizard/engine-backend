module WizardLib.Public.Api.Resource.User.UserLocaleDTO where

import qualified Data.UUID as U
import GHC.Generics

data UserLocaleDTO = UserLocaleDTO
  { uuid :: Maybe U.UUID
  }
  deriving (Generic)

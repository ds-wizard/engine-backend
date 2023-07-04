module WizardLib.Public.Api.Resource.UserToken.ApiKeyCreateDTO where

import Data.Time
import GHC.Generics

data ApiKeyCreateDTO = ApiKeyCreateDTO
  { name :: String
  , expiresAt :: UTCTime
  }
  deriving (Generic)

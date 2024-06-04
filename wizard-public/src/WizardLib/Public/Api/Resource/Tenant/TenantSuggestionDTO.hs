module WizardLib.Public.Api.Resource.Tenant.TenantSuggestionDTO where

import qualified Data.UUID as U
import GHC.Generics

data TenantSuggestionDTO = TenantSuggestionDTO
  { uuid :: U.UUID
  , name :: String
  , logoUrl :: Maybe String
  , primaryColor :: Maybe String
  , clientUrl :: String
  }
  deriving (Show, Eq, Generic)

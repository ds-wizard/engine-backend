module WizardLib.Public.Model.Tenant.TenantSuggestion where

import qualified Data.UUID as U
import GHC.Generics

data TenantSuggestion = TenantSuggestion
  { uuid :: U.UUID
  , name :: String
  , clientUrl :: String
  , primaryColor :: Maybe String
  , logoUrl :: Maybe String
  }
  deriving (Show, Eq, Generic)

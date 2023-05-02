module Wizard.Model.Registry.RegistryOrganization where

import Data.Time
import GHC.Generics

data RegistryOrganization = RegistryOrganization
  { organizationId :: String
  , name :: String
  , logo :: Maybe String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

module Wizard.Model.Registry.RegistryTemplate where

import Data.Time
import GHC.Generics

data RegistryTemplate = RegistryTemplate
  { organizationId :: String
  , templateId :: String
  , remoteVersion :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

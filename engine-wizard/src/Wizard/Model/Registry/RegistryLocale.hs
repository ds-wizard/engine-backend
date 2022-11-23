module Wizard.Model.Registry.RegistryLocale where

import Data.Time
import GHC.Generics

data RegistryLocale = RegistryLocale
  { organizationId :: String
  , localeId :: String
  , remoteVersion :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

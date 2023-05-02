module Wizard.Model.Registry.RegistryPackage where

import Data.Time
import GHC.Generics

data RegistryPackage = RegistryPackage
  { organizationId :: String
  , kmId :: String
  , remoteVersion :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

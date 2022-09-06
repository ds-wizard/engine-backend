module Wizard.Model.Registry.RegistryPackage where

import Data.Time
import GHC.Generics

data RegistryPackage =
  RegistryPackage
    { _registryPackageOrganizationId :: String
    , _registryPackageKmId :: String
    , _registryPackageRemoteVersion :: String
    , _registryPackageCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

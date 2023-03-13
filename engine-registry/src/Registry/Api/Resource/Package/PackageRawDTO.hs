module Registry.Api.Resource.Package.PackageRawDTO where

import Data.Aeson
import Data.Time
import GHC.Generics

import Shared.Model.Package.Package

data PackageRawDTO = PackageRawDTO
  { pId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , phase :: PackagePhase
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , previousPackageId :: Maybe String
  , forkOfPackageId :: Maybe String
  , mergeCheckpointPackageId :: Maybe String
  , events :: Value
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

module Shared.Model.Package.Package where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data PackagePhase
  = ReleasedPackagePhase
  | DeprecatedPackagePhase
  deriving (Show, Eq, Generic, Read)

data Package = Package
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
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord Package where
  compare a b =
    compare a.organizationId b.organizationId
      <> compare a.kmId b.kmId
      <> compare a.version b.version

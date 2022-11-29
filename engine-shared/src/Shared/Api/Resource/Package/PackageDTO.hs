module Shared.Api.Resource.Package.PackageDTO where

import Data.Time
import GHC.Generics

import Shared.Model.Event.Event

data PackageDTO = PackageDTO
  { pId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , previousPackageId :: Maybe String
  , forkOfPackageId :: Maybe String
  , mergeCheckpointPackageId :: Maybe String
  , events :: [Event]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

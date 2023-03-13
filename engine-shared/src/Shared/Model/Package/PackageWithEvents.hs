module Shared.Model.Package.PackageWithEvents where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event
import Shared.Model.Package.Package

data PackageWithEvents = PackageWithEvents
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
  , events :: [Event]
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

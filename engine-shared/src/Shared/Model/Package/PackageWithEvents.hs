module Shared.Model.Package.PackageWithEvents where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event

data PackageWithEvents =
  PackageWithEvents
    { _packageWithEventsPId :: String
    , _packageWithEventsName :: String
    , _packageWithEventsOrganizationId :: String
    , _packageWithEventsKmId :: String
    , _packageWithEventsVersion :: String
    , _packageWithEventsMetamodelVersion :: Int
    , _packageWithEventsDescription :: String
    , _packageWithEventsReadme :: String
    , _packageWithEventsLicense :: String
    , _packageWithEventsPreviousPackageId :: Maybe String
    , _packageWithEventsForkOfPackageId :: Maybe String
    , _packageWithEventsMergeCheckpointPackageId :: Maybe String
    , _packageWithEventsEvents :: [Event]
    , _packageWithEventsAppUuid :: U.UUID
    , _packageWithEventsCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

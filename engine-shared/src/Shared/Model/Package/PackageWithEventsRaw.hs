module Shared.Model.Package.PackageWithEventsRaw where

import Data.Aeson
import Data.Time
import GHC.Generics

data PackageWithEventsRaw =
  PackageWithEventsRaw
    { _packageWithEventsRawPId :: String
    , _packageWithEventsRawName :: String
    , _packageWithEventsRawOrganizationId :: String
    , _packageWithEventsRawKmId :: String
    , _packageWithEventsRawVersion :: String
    , _packageWithEventsRawMetamodelVersion :: Int
    , _packageWithEventsRawDescription :: String
    , _packageWithEventsRawReadme :: String
    , _packageWithEventsRawLicense :: String
    , _packageWithEventsRawPreviousPackageId :: Maybe String
    , _packageWithEventsRawForkOfPackageId :: Maybe String
    , _packageWithEventsRawMergeCheckpointPackageId :: Maybe String
    , _packageWithEventsRawEvents :: Value
    , _packageWithEventsRawCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

module Model.Package.Package where

import Data.Time
import GHC.Generics

data Package = Package
  { _packagePId :: String
  , _packageName :: String
  , _packageOrganizationId :: String
  , _packageKmId :: String
  , _packageVersion :: String
  , _packageDescription :: String
  , _packageReadme :: String
  , _packageLicense :: String
  , _packageMetamodelVersion :: Int
  , _packagePreviousPackageId :: Maybe String
  , _packageForkOfPackageId :: Maybe String
  , _packageMergeCheckpointPackageId :: Maybe String
  , _packageCreatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

module Api.Resource.Package.PackageDTO where

import Data.Time
import GHC.Generics

import Api.Resource.Event.EventDTO

data PackageDTO = PackageDTO
  { _packageDTOPId :: String
  , _packageDTOName :: String
  , _packageDTOOrganizationId :: String
  , _packageDTOKmId :: String
  , _packageDTOVersion :: String
  , _packageDTOMetamodelVersion :: Int
  , _packageDTODescription :: String
  , _packageDTOReadme :: String
  , _packageDTOLicense :: String
  , _packageDTOPreviousPackageId :: Maybe String
  , _packageDTOForkOfPackageId :: Maybe String
  , _packageDTOMergeCheckpointPackageId :: Maybe String
  , _packageDTOEvents :: [EventDTO]
  , _packageDTOCreatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

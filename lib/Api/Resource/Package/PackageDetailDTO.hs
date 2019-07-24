module Api.Resource.Package.PackageDetailDTO where

import Data.Time
import GHC.Generics

import Api.Resource.Organization.OrganizationSimpleDTO
import Model.Package.PackageState

data PackageDetailDTO = PackageDetailDTO
  { _packageDetailDTOPId :: String
  , _packageDetailDTOName :: String
  , _packageDetailDTOOrganizationId :: String
  , _packageDetailDTOKmId :: String
  , _packageDetailDTOVersion :: String
  , _packageDetailDTODescription :: String
  , _packageDetailDTOReadme :: String
  , _packageDetailDTOLicense :: String
  , _packageDetailDTOMetamodelVersion :: Int
  , _packageDetailDTOPreviousPackageId :: Maybe String
  , _packageDetailDTOForkOfPackageId :: Maybe String
  , _packageDetailDTOMergeCheckpointPackageId :: Maybe String
  , _packageDetailDTOVersions :: [String]
  , _packageDetailDTORemoteLatestVersion :: Maybe String
  , _packageDetailDTOOrganization :: Maybe OrganizationSimpleDTO
  , _packageDetailDTORegistryLink :: Maybe String
  , _packageDetailDTOState :: PackageState
  , _packageDetailDTOCreatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

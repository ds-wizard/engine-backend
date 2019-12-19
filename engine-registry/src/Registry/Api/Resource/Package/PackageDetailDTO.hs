module Registry.Api.Resource.Package.PackageDetailDTO where

import Data.Time
import GHC.Generics

import Registry.Api.Resource.Organization.OrganizationSimpleDTO

data PackageDetailDTO =
  PackageDetailDTO
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
    , _packageDetailDTOOrganization :: OrganizationSimpleDTO
    , _packageDetailDTOCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

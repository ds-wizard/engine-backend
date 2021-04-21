module Registry.Api.Resource.Package.PackageRawDTO where

import Data.Aeson
import Data.Time
import GHC.Generics

data PackageRawDTO =
  PackageRawDTO
    { _packageRawDTOPId :: String
    , _packageRawDTOName :: String
    , _packageRawDTOOrganizationId :: String
    , _packageRawDTOKmId :: String
    , _packageRawDTOVersion :: String
    , _packageRawDTOMetamodelVersion :: Int
    , _packageRawDTODescription :: String
    , _packageRawDTOReadme :: String
    , _packageRawDTOLicense :: String
    , _packageRawDTOPreviousPackageId :: Maybe String
    , _packageRawDTOForkOfPackageId :: Maybe String
    , _packageRawDTOMergeCheckpointPackageId :: Maybe String
    , _packageRawDTOEvents :: Value
    , _packageRawDTOCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

module Integration.Resource.Package.PackageDetailIDTO where

import Data.Time

import Integration.Resource.Organization.OrganizationSimpleIDTO

data PackageDetailIDTO = PackageDetailIDTO
  { _packageDetailIDTOPId :: String
  , _packageDetailIDTOName :: String
  , _packageDetailIDTOOrganizationId :: String
  , _packageDetailIDTOKmId :: String
  , _packageDetailIDTOVersion :: String
  , _packageDetailIDTODescription :: String
  , _packageDetailIDTOReadme :: String
  , _packageDetailIDTOLicense :: String
  , _packageDetailIDTOMetamodelVersion :: Int
  , _packageDetailIDTOPreviousPackageId :: Maybe String
  , _packageDetailIDTOForkOfPackageId :: Maybe String
  , _packageDetailIDTOMergeCheckpointPackageId :: Maybe String
  , _packageDetailIDTOVersions :: [String]
  , _packageDetailIDTOOrganization :: OrganizationSimpleIDTO
  , _packageDetailIDTOCreatedAt :: UTCTime
  } deriving (Show, Eq)

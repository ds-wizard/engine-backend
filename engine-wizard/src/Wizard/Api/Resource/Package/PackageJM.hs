module Wizard.Api.Resource.Package.PackageJM where

import Control.Monad
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time

import Shared.Util.JSON (simpleToJSON)
import Wizard.Api.Resource.Event.EventJM ()
import Wizard.Api.Resource.Package.PackageDTO

instance ToJSON PackageDTO where
  toJSON = simpleToJSON "_packageDTO"

instance FromJSON PackageDTO where
  parseJSON (Object o) = do
    _packageDTOPId <- o .: "id"
    _packageDTOName <- o .: "name"
    _packageDTOOrganizationId <- o .: "organizationId"
    _packageDTOKmId <- o .: "kmId"
    _packageDTOVersion <- o .: "version"
    _packageDTOMetamodelVersion <- o .: "metamodelVersion"
    _packageDTODescription <- o .: "description"
    _packageDTOReadme <- o .:? "readme" .!= ""
    _packageDTOLicense <- o .:? "license" .!= ""
    _packageDTOParentPackageId <- o .:? "parentPackageId"
    _packageDTOPreviousPackageId <- o .:? "previousPackageId" .!= _packageDTOParentPackageId
    _packageDTOForkOfPackageId <- o .:? "forkOfPackageId" .!= _packageDTOParentPackageId
    _packageDTOMergeCheckpointPackageId <- o .:? "mergeCheckpointPackageId" .!= _packageDTOParentPackageId
    eventSerialized <- o .: "events"
    _packageDTOEvents <- parseJSON eventSerialized
    _packageDTOCreatedAt <- o .:? "createdAt" .!= (UTCTime (fromJust $ fromGregorianValid 1970 1 1) 0)
    return PackageDTO {..}
  parseJSON _ = mzero

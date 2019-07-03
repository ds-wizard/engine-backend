module Api.Resource.Package.PackageJM where

import Control.Monad
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time

import Api.Resource.Event.EventJM ()
import Api.Resource.Package.PackageDTO

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
    _packageDTOParentPackageId <- o .: "parentPackageId"
    eventSerialized <- o .: "events"
    _packageDTOEvents <- parseJSON eventSerialized
    _packageDTOCreatedAt <- o .:? "createdAt" .!= (UTCTime (fromJust $ fromGregorianValid 1970 1 1) 0)
    return PackageDTO {..}
  parseJSON _ = mzero

instance ToJSON PackageDTO where
  toJSON PackageDTO {..} =
    object
      [ "id" .= _packageDTOPId
      , "name" .= _packageDTOName
      , "organizationId" .= _packageDTOOrganizationId
      , "kmId" .= _packageDTOKmId
      , "version" .= _packageDTOVersion
      , "metamodelVersion" .= _packageDTOMetamodelVersion
      , "description" .= _packageDTODescription
      , "readme" .= _packageDTOReadme
      , "license" .= _packageDTOLicense
      , "parentPackageId" .= _packageDTOParentPackageId
      , "events" .= toJSON _packageDTOEvents
      , "createdAt" .= _packageDTOCreatedAt
      ]

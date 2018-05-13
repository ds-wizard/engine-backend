module Api.Resource.Package.PackageWithEventsDTO where

import Control.Monad
import Data.Aeson

import Api.Resource.Event.EventDTO

data PackageWithEventsDTO = PackageWithEventsDTO
  { _packageWithEventsDTOPId :: String
  , _packageWithEventsDTOName :: String
  , _packageWithEventsDTOOrganizationId :: String
  , _packageWithEventsDTOArtifactId :: String
  , _packageWithEventsDTOVersion :: String
  , _packageWithEventsDTODescription :: String
  , _packageWithEventsDTOParentPackageId :: Maybe String
  , _packageWithEventsDTOEvents :: [EventDTO]
  } deriving (Show, Eq)

instance FromJSON PackageWithEventsDTO where
  parseJSON (Object o) = do
    _packageWithEventsDTOPId <- o .: "id"
    _packageWithEventsDTOName <- o .: "name"
    _packageWithEventsDTOOrganizationId <- o .: "organizationId"
    _packageWithEventsDTOArtifactId <- o .: "artifactId"
    _packageWithEventsDTOVersion <- o .: "version"
    _packageWithEventsDTODescription <- o .: "description"
    _packageWithEventsDTOParentPackageId <- o .: "parentPackageId"
    eventSerialized <- o .: "events"
    _packageWithEventsDTOEvents <- parseJSON eventSerialized
    return PackageWithEventsDTO {..}
  parseJSON _ = mzero

instance ToJSON PackageWithEventsDTO where
  toJSON PackageWithEventsDTO {..} =
    object
      [ "id" .= _packageWithEventsDTOPId
      , "name" .= _packageWithEventsDTOName
      , "organizationId" .= _packageWithEventsDTOOrganizationId
      , "artifactId" .= _packageWithEventsDTOArtifactId
      , "version" .= _packageWithEventsDTOVersion
      , "description" .= _packageWithEventsDTODescription
      , "parentPackageId" .= _packageWithEventsDTOParentPackageId
      , "events" .= toJSON _packageWithEventsDTOEvents
      ]

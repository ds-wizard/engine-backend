module Api.Resources.Package.PackageWithEventsDTO where

import Control.Lens ((^.), makeLenses)
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID

import Api.Resources.Event.EventDTO
import Common.Types
import Common.Uuid

data PackageWithEventsDTO = PackageWithEventsDTO
  { _pkgwedtoId :: String
  , _pkgwedtoName :: String
  , _pkgwedtoGroupId :: String
  , _pkgwedtoArtifactId :: String
  , _pkgwedtoVersion :: String
  , _pkgwedtoDescription :: String
  , _pkgwedtoParentPackageId :: Maybe String
  , _pkgwedtoEvents :: [EventDTO]
  } deriving (Show, Eq)

makeLenses ''PackageWithEventsDTO

instance FromJSON PackageWithEventsDTO where
  parseJSON (Object o) = do
    _pkgwedtoId <- o .: "id"
    _pkgwedtoName <- o .: "name"
    _pkgwedtoGroupId <- o .: "groupId"
    _pkgwedtoArtifactId <- o .: "artifactId"
    _pkgwedtoVersion <- o .: "version"
    _pkgwedtoDescription <- o .: "description"
    _pkgwedtoParentPackageId <- o .: "parentPackageId"
    eventSerialized <- o .: "events"
    _pkgwedtoEvents <- parseJSON eventSerialized
    return PackageWithEventsDTO {..}
  parseJSON _ = mzero

instance ToJSON PackageWithEventsDTO where
  toJSON PackageWithEventsDTO {..} =
    object
      [ "id" .= _pkgwedtoId
      , "name" .= _pkgwedtoName
      , "groupId" .= _pkgwedtoGroupId
      , "artifactId" .= _pkgwedtoArtifactId
      , "version" .= _pkgwedtoVersion
      , "description" .= _pkgwedtoDescription
      , "parentPackageId" .= _pkgwedtoParentPackageId
      , "events" .= toJSON _pkgwedtoEvents
      ]

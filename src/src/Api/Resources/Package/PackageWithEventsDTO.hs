module Api.Resources.Package.PackageWithEventsDTO where

import Control.Lens (makeLenses, (^.))
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
  , _pkgwedtoShortName :: String
  , _pkgwedtoVersion :: String
  , _pkgwedtoDescription :: String
  , _pkgwedtoParentPackage :: Maybe PackageWithEventsDTO
  , _pkgwedtoEvents :: [EventDTO]
  } deriving (Show, Eq)

makeLenses ''PackageWithEventsDTO

instance FromJSON PackageWithEventsDTO where
  parseJSON (Object o) = do
    _pkgwedtoId <- o .: "packageId"
    _pkgwedtoName <- o .: "name"
    _pkgwedtoShortName <- o .: "shortName"
    _pkgwedtoVersion <- o .: "version"
    _pkgwedtoDescription <- o .: "description"
    _pkgwedtoParentPackage <- o .: "parentPackage"
    eventSerialized <- o .: "events"
    _pkgwedtoEvents <- parseJSON eventSerialized
    return PackageWithEventsDTO {..}
  parseJSON _ = mzero

instance ToJSON PackageWithEventsDTO where
  toJSON PackageWithEventsDTO {..} =
    object
      [ "packageId" .= _pkgwedtoId
      , "name" .= _pkgwedtoName
      , "shortName" .= _pkgwedtoShortName
      , "version" .= _pkgwedtoVersion
      , "description" .= _pkgwedtoDescription
      , "parentPackage" .= _pkgwedtoParentPackage
      , "events" .= toJSON _pkgwedtoEvents
      ]

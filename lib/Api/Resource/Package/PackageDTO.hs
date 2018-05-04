module Api.Resource.Package.PackageDTO where

import Control.Lens ((^.), makeLenses)
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID

import Common.Types
import Common.Uuid

data PackageDTO = PackageDTO
  { _pkgdtoId :: String
  , _pkgdtoName :: String
  , _pkgdtoGroupId :: String
  , _pkgdtoArtifactId :: String
  , _pkgdtoVersion :: String
  , _pkgdtoDescription :: String
  , _pkgdtoParentPackageId :: Maybe String
  } deriving (Show, Eq)

makeLenses ''PackageDTO

instance ToJSON PackageDTO where
  toJSON PackageDTO {..} =
    object
      [ "id" .= _pkgdtoId
      , "name" .= _pkgdtoName
      , "groupId" .= _pkgdtoGroupId
      , "artifactId" .= _pkgdtoArtifactId
      , "version" .= _pkgdtoVersion
      , "description" .= _pkgdtoDescription
      , "parentPackageId" .= _pkgdtoParentPackageId
      ]

instance FromJSON PackageDTO where
  parseJSON (Object o) = do
    _pkgdtoId <- o .: "id"
    _pkgdtoName <- o .: "name"
    _pkgdtoGroupId <- o .: "groupId"
    _pkgdtoArtifactId <- o .: "artifactId"
    _pkgdtoVersion <- o .: "version"
    _pkgdtoDescription <- o .: "description"
    _pkgdtoParentPackageId <- o .: "parentPackageId"
    return PackageDTO {..}
  parseJSON _ = mzero

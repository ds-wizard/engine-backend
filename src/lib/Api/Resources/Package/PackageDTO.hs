module Api.Resources.Package.PackageDTO where

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

module Api.Resource.Package.PackageSimpleDTO where

import Control.Lens (makeLenses)
import Data.Aeson

data PackageSimpleDTO = PackageSimpleDTO
  { _pkgsdtoName :: String
  , _pkgsdtoGroupId :: String
  , _pkgsdtoArtifactId :: String
  } deriving (Show, Eq)

makeLenses ''PackageSimpleDTO

instance ToJSON PackageSimpleDTO where
  toJSON PackageSimpleDTO {..} =
    object ["name" .= _pkgsdtoName, "groupId" .= _pkgsdtoGroupId, "artifactId" .= _pkgsdtoArtifactId]

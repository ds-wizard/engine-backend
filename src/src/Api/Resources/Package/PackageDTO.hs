module Api.Resources.Package.PackageDTO where

import Control.Lens (makeLenses, (^.))
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
  , _pkgdtoArtefactId :: String
  , _pkgdtoVersion :: String
  , _pkgdtoDescription :: String
  , _pkgdtoParentPackage :: Maybe PackageDTO
  } deriving (Show, Eq)

makeLenses ''PackageDTO

instance ToJSON PackageDTO where
  toJSON PackageDTO {..} =
    object
      [ "packageId" .= _pkgdtoId
      , "name" .= _pkgdtoName
      , "groupId" .= _pkgdtoGroupId
      , "artefactId" .= _pkgdtoArtefactId
      , "version" .= _pkgdtoVersion
      , "description" .= _pkgdtoDescription
      , "parentPackge" .= _pkgdtoParentPackage
      ]

module Api.Resources.Package.PackageSimpleDTO where

import Control.Lens (makeLenses, (^.))
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID

import Common.Types
import Common.Uuid

data PackageSimpleDTO = PackageSimpleDTO
  { _pkgsdtoName :: String
  , _pkgsdtoGroupId :: String
  , _pkgsdtoArtefactId :: String
  } deriving (Show, Eq)

makeLenses ''PackageSimpleDTO

instance ToJSON PackageSimpleDTO where
  toJSON PackageSimpleDTO {..} =
    object
      [ "name" .= _pkgsdtoName
      , "groupId" .= _pkgsdtoGroupId
      , "artefactId" .= _pkgsdtoArtefactId
      ]

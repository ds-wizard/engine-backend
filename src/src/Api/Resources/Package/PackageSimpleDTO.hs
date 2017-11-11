module Api.Resources.Package.PackageSimpleDTO where

import Control.Lens (makeLenses, (^.))
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID

import Common.Types
import Common.Uuid

data PackageSimpleDTO = PackageSimpleDTO
  { _pkgsdtoId :: String
  , _pkgsdtoName :: String
  , _pkgsdtoShortName :: String
  } deriving (Show, Eq)

makeLenses ''PackageSimpleDTO

instance ToJSON PackageSimpleDTO where
  toJSON PackageSimpleDTO {..} =
    object
      [ "packageId" .= _pkgsdtoId
      , "name" .= _pkgsdtoName
      , "shortName" .= _pkgsdtoShortName
      ]

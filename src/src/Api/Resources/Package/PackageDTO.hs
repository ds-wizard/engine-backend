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
  , _pkgdtoShortName :: String
  , _pkgdtoVersion :: String
  , _pkgdtoParentPackage :: Maybe PackageDTO
  } deriving (Show, Eq)

makeLenses ''PackageDTO

--instance FromJSON PackageDTO where
--  parseJSON (Object o) = do
--    _pkgdtoId <- o .: "packageId"
--    _pkgdtoName <- o .: "name"
--    _pkgdtoShortName <- o .: "shortName"
--    _pkgdtoVersion <- o .: "version"
--    _pkgdtoParentPackage <- o .: "parentPackge"
--    return PackageDTO {..}
--  parseJSON _ = mzero
instance ToJSON PackageDTO where
  toJSON PackageDTO {..} =
    object
      [ "packageId" .= _pkgdtoId
      , "name" .= _pkgdtoName
      , "shortName" .= _pkgdtoShortName
      , "version" .= _pkgdtoVersion
      , "parentPackge" .= _pkgdtoParentPackage
      ]

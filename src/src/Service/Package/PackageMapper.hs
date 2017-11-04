module Service.Package.PackageMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resources.Package.PackageDTO
import Common.Types
import Model.Package.Package

toDTO :: Package -> PackageDTO
toDTO package =
  PackageDTO
  { _pkgdtoId = package ^. pkgId
  , _pkgdtoName = package ^. pkgName
  , _pkgdtoShortName = package ^. pkgShortName
  , _pkgdtoVersion = package ^. pkgVersion
  , _pkgdtoParentPackage = toDTO <$> (package ^. pkgParentPackage)
  }

fromDTO :: PackageDTO -> Package
fromDTO dto =
  Package
  { _pkgId = dto ^. pkgdtoId
  , _pkgName = dto ^. pkgdtoName
  , _pkgShortName = dto ^. pkgdtoShortName
  , _pkgVersion = dto ^. pkgdtoVersion
  , _pkgParentPackage = fromDTO <$> (dto ^. pkgdtoParentPackage)
  }

buildPackage :: String -> String -> String -> Maybe PackageDTO -> Package
buildPackage name shortName version maybeparentPackage =
  Package
  { _pkgId = shortName ++ ":" ++ version
  , _pkgName = name
  , _pkgShortName = shortName
  , _pkgVersion = version
  , _pkgParentPackage = fromDTO <$> maybeparentPackage
  }

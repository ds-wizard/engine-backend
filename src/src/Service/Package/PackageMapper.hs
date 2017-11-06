module Service.Package.PackageMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resources.Package.PackageDTO
import Api.Resources.Package.PackageWithEventsDTO
import Common.Types
import Model.Event.Event
import Model.Package.Package
import Service.Event.EventMapper

packageToDTO :: Package -> PackageDTO
packageToDTO package =
  PackageDTO
  { _pkgdtoId = package ^. pkgId
  , _pkgdtoName = package ^. pkgName
  , _pkgdtoShortName = package ^. pkgShortName
  , _pkgdtoVersion = package ^. pkgVersion
  , _pkgdtoParentPackage = packageToDTO <$> (package ^. pkgParentPackage)
  }

packageWithEventsToDTO :: PackageWithEvents -> PackageDTO
packageWithEventsToDTO package =
  PackageDTO
  { _pkgdtoId = package ^. pkgweId
  , _pkgdtoName = package ^. pkgweName
  , _pkgdtoShortName = package ^. pkgweShortName
  , _pkgdtoVersion = package ^. pkgweVersion
  , _pkgdtoParentPackage =
      packageWithEventsToDTO <$> (package ^. pkgweParentPackage)
  }

packageWithEventsToDTOWithEvents :: PackageWithEvents -> PackageWithEventsDTO
packageWithEventsToDTOWithEvents package =
  PackageWithEventsDTO
  { _pkgwedtoId = package ^. pkgweId
  , _pkgwedtoName = package ^. pkgweName
  , _pkgwedtoShortName = package ^. pkgweShortName
  , _pkgwedtoVersion = package ^. pkgweVersion
  , _pkgwedtoParentPackage =
      packageWithEventsToDTOWithEvents <$> (package ^. pkgweParentPackage)
  , _pkgwedtoEvents = toDTOs (package ^. pkgweEvents)
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

fromDTOWithEvents :: PackageWithEventsDTO -> PackageWithEvents
fromDTOWithEvents dto =
  PackageWithEvents
  { _pkgweId = dto ^. pkgwedtoId
  , _pkgweName = dto ^. pkgwedtoName
  , _pkgweShortName = dto ^. pkgwedtoShortName
  , _pkgweVersion = dto ^. pkgwedtoVersion
  , _pkgweParentPackage = fromDTOWithEvents <$> (dto ^. pkgwedtoParentPackage)
  , _pkgweEvents = fromDTOs (dto ^. pkgwedtoEvents)
  }

buildPackage
  :: String
  -> String
  -> String
  -> Maybe PackageWithEvents
  -> [Event]
  -> PackageWithEvents
buildPackage name shortName version maybeParentPackage events =
  PackageWithEvents
  { _pkgweId = shortName ++ ":" ++ version
  , _pkgweName = name
  , _pkgweShortName = shortName
  , _pkgweVersion = version
  , _pkgweParentPackage = maybeParentPackage
  , _pkgweEvents = events
  }

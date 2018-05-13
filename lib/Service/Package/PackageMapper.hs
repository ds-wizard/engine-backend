module Service.Package.PackageMapper where

import Control.Lens ((^.))

import Api.Resource.Package.PackageDTO
import Api.Resource.Package.PackageSimpleDTO
import Api.Resource.Package.PackageWithEventsDTO
import LensesConfig
import Model.Event.Event
import Model.Package.Package
import Service.Event.EventMapper

packageToDTO :: Package -> PackageDTO
packageToDTO package =
  PackageDTO
  { _packageDTOPId = package ^. pId
  , _packageDTOName = package ^. name
  , _packageDTOOrganizationId = package ^. organizationId
  , _packageDTOArtifactId = package ^. artifactId
  , _packageDTOVersion = package ^. version
  , _packageDTODescription = package ^. description
  , _packageDTOParentPackageId = package ^. parentPackageId
  }

packageToSimpleDTO :: Package -> PackageSimpleDTO
packageToSimpleDTO package =
  PackageSimpleDTO
  { _packageSimpleDTOName = package ^. name
  , _packageSimpleDTOOrganizationId = package ^. organizationId
  , _packageSimpleDTOArtifactId = package ^. artifactId
  }

packageWithEventsToDTO :: PackageWithEvents -> PackageDTO
packageWithEventsToDTO package =
  PackageDTO
  { _packageDTOPId = package ^. pId
  , _packageDTOName = package ^. name
  , _packageDTOOrganizationId = package ^. organizationId
  , _packageDTOArtifactId = package ^. artifactId
  , _packageDTOVersion = package ^. version
  , _packageDTODescription = package ^. description
  , _packageDTOParentPackageId = package ^. parentPackageId
  }

packageWithEventsToDTOWithEvents :: PackageWithEvents -> PackageWithEventsDTO
packageWithEventsToDTOWithEvents package =
  PackageWithEventsDTO
  { _packageWithEventsDTOPId = package ^. pId
  , _packageWithEventsDTOName = package ^. name
  , _packageWithEventsDTOOrganizationId = package ^. organizationId
  , _packageWithEventsDTOArtifactId = package ^. artifactId
  , _packageWithEventsDTOVersion = package ^. version
  , _packageWithEventsDTODescription = package ^. description
  , _packageWithEventsDTOParentPackageId = package ^. parentPackageId
  , _packageWithEventsDTOEvents = toDTOs (package ^. events)
  }

fromDTO :: PackageDTO -> Package
fromDTO dto =
  Package
  { _packagePId = dto ^. pId
  , _packageName = dto ^. name
  , _packageOrganizationId = dto ^. organizationId
  , _packageArtifactId = dto ^. artifactId
  , _packageVersion = dto ^. version
  , _packageDescription = dto ^. description
  , _packageParentPackageId = dto ^. parentPackageId
  }

fromDTOWithEvents :: PackageWithEventsDTO -> PackageWithEvents
fromDTOWithEvents dto =
  PackageWithEvents
  { _packageWithEventsPId = dto ^. pId
  , _packageWithEventsName = dto ^. name
  , _packageWithEventsOrganizationId = dto ^. organizationId
  , _packageWithEventsArtifactId = dto ^. artifactId
  , _packageWithEventsVersion = dto ^. version
  , _packageWithEventsDescription = dto ^. description
  , _packageWithEventsParentPackageId = dto ^. parentPackageId
  , _packageWithEventsEvents = fromDTOs (dto ^. events)
  }

buildPackageId :: String -> String -> String -> String
buildPackageId pkgOrganizationId pkgArtifactId pkgVersion = pkgOrganizationId ++ ":" ++ pkgArtifactId ++ ":" ++ pkgVersion

buildPackage :: String -> String -> String -> String -> String -> Maybe String -> [Event] -> PackageWithEvents
buildPackage pkgName pkgOrganizationId pkgArtifactId pkgVersion pkgDescription pkgMaybeParentPackageId pkgEvents =
  PackageWithEvents
  { _packageWithEventsPId = buildPackageId pkgOrganizationId pkgArtifactId pkgVersion
  , _packageWithEventsName = pkgName
  , _packageWithEventsOrganizationId = pkgOrganizationId
  , _packageWithEventsArtifactId = pkgArtifactId
  , _packageWithEventsVersion = pkgVersion
  , _packageWithEventsDescription = pkgDescription
  , _packageWithEventsParentPackageId = pkgMaybeParentPackageId
  , _packageWithEventsEvents = pkgEvents
  }

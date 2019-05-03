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
  , _packageDTOKmId = package ^. kmId
  , _packageDTOVersion = package ^. version
  , _packageDTOMetamodelVersion = package ^. metamodelVersion
  , _packageDTODescription = package ^. description
  , _packageDTOParentPackageId = package ^. parentPackageId
  }

packageToSimpleDTO :: Package -> PackageSimpleDTO
packageToSimpleDTO package =
  PackageSimpleDTO
  { _packageSimpleDTOName = package ^. name
  , _packageSimpleDTOOrganizationId = package ^. organizationId
  , _packageSimpleDTOKmId = package ^. kmId
  , _packageSimpleDTOLatestVersion = package ^. version
  }

packageWithEventsToDTO :: PackageWithEvents -> PackageDTO
packageWithEventsToDTO package =
  PackageDTO
  { _packageDTOPId = package ^. pId
  , _packageDTOName = package ^. name
  , _packageDTOOrganizationId = package ^. organizationId
  , _packageDTOKmId = package ^. kmId
  , _packageDTOVersion = package ^. version
  , _packageDTOMetamodelVersion = package ^. metamodelVersion
  , _packageDTODescription = package ^. description
  , _packageDTOParentPackageId = package ^. parentPackageId
  }

packageWithEventsToSimpleDTO :: PackageWithEvents -> PackageSimpleDTO
packageWithEventsToSimpleDTO package =
  PackageSimpleDTO
  { _packageSimpleDTOName = package ^. name
  , _packageSimpleDTOOrganizationId = package ^. organizationId
  , _packageSimpleDTOKmId = package ^. kmId
  , _packageSimpleDTOLatestVersion = package ^. version
  }

packageWithEventsToDTOWithEvents :: PackageWithEvents -> PackageWithEventsDTO
packageWithEventsToDTOWithEvents package =
  PackageWithEventsDTO
  { _packageWithEventsDTOPId = package ^. pId
  , _packageWithEventsDTOName = package ^. name
  , _packageWithEventsDTOOrganizationId = package ^. organizationId
  , _packageWithEventsDTOKmId = package ^. kmId
  , _packageWithEventsDTOVersion = package ^. version
  , _packageWithEventsDTOMetamodelVersion = package ^. metamodelVersion
  , _packageWithEventsDTODescription = package ^. description
  , _packageWithEventsDTOParentPackageId = package ^. parentPackageId
  , _packageWithEventsDTOEvents = toDTOs (package ^. events)
  }

packageWithEventsToPackage :: PackageWithEvents -> Package
packageWithEventsToPackage package =
  Package
  { _packagePId = package ^. pId
  , _packageName = package ^. name
  , _packageOrganizationId = package ^. organizationId
  , _packageKmId = package ^. kmId
  , _packageVersion = package ^. version
  , _packageMetamodelVersion = package ^. metamodelVersion
  , _packageDescription = package ^. description
  , _packageParentPackageId = package ^. parentPackageId
  }

fromDTO :: PackageDTO -> Package
fromDTO dto =
  Package
  { _packagePId = dto ^. pId
  , _packageName = dto ^. name
  , _packageOrganizationId = dto ^. organizationId
  , _packageKmId = dto ^. kmId
  , _packageVersion = dto ^. version
  , _packageMetamodelVersion = dto ^. metamodelVersion
  , _packageDescription = dto ^. description
  , _packageParentPackageId = dto ^. parentPackageId
  }

fromDTOWithEvents :: PackageWithEventsDTO -> PackageWithEvents
fromDTOWithEvents dto =
  PackageWithEvents
  { _packageWithEventsPId = dto ^. pId
  , _packageWithEventsName = dto ^. name
  , _packageWithEventsOrganizationId = dto ^. organizationId
  , _packageWithEventsKmId = dto ^. kmId
  , _packageWithEventsVersion = dto ^. version
  , _packageWithEventsMetamodelVersion = dto ^. metamodelVersion
  , _packageWithEventsDescription = dto ^. description
  , _packageWithEventsParentPackageId = dto ^. parentPackageId
  , _packageWithEventsEvents = fromDTOs (dto ^. events)
  }

buildPackageId :: String -> String -> String -> String
buildPackageId pkgOrganizationId pkgKmId pkgVersion = pkgOrganizationId ++ ":" ++ pkgKmId ++ ":" ++ pkgVersion

buildPackage :: String -> String -> String -> String -> Int -> String -> Maybe String -> [Event] -> PackageWithEvents
buildPackage pkgName pkgOrganizationId pkgKmId pkgVersion pkgMetamodelVersion pkgDescription pkgMaybeParentPackageId pkgEvents =
  PackageWithEvents
  { _packageWithEventsPId = buildPackageId pkgOrganizationId pkgKmId pkgVersion
  , _packageWithEventsName = pkgName
  , _packageWithEventsOrganizationId = pkgOrganizationId
  , _packageWithEventsKmId = pkgKmId
  , _packageWithEventsVersion = pkgVersion
  , _packageWithEventsMetamodelVersion = pkgMetamodelVersion
  , _packageWithEventsDescription = pkgDescription
  , _packageWithEventsParentPackageId = pkgMaybeParentPackageId
  , _packageWithEventsEvents = pkgEvents
  }

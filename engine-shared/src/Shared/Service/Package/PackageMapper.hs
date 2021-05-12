module Shared.Service.Package.PackageMapper where

import Control.Lens ((^.))
import qualified Data.List as L

import LensesConfig
import Shared.Api.Resource.Package.PackageDTO
import Shared.Api.Resource.Package.PackageSuggestionDTO
import Shared.Model.Common.Page
import Shared.Model.Package.Package
import Shared.Model.Package.PackageGroup
import Shared.Model.Package.PackageWithEvents
import Shared.Util.String (splitOn)

toPackage :: PackageWithEvents -> Package
toPackage pkg =
  Package
    { _packagePId = pkg ^. pId
    , _packageName = pkg ^. name
    , _packageOrganizationId = pkg ^. organizationId
    , _packageKmId = pkg ^. kmId
    , _packageVersion = pkg ^. version
    , _packageMetamodelVersion = pkg ^. metamodelVersion
    , _packageDescription = pkg ^. description
    , _packageReadme = pkg ^. readme
    , _packageLicense = pkg ^. license
    , _packagePreviousPackageId = pkg ^. previousPackageId
    , _packageForkOfPackageId = pkg ^. forkOfPackageId
    , _packageMergeCheckpointPackageId = pkg ^. mergeCheckpointPackageId
    , _packageCreatedAt = pkg ^. createdAt
    }

toDTO :: PackageWithEvents -> PackageDTO
toDTO pkg =
  PackageDTO
    { _packageDTOPId = pkg ^. pId
    , _packageDTOName = pkg ^. name
    , _packageDTOOrganizationId = pkg ^. organizationId
    , _packageDTOKmId = pkg ^. kmId
    , _packageDTOVersion = pkg ^. version
    , _packageDTOMetamodelVersion = pkg ^. metamodelVersion
    , _packageDTODescription = pkg ^. description
    , _packageDTOReadme = pkg ^. readme
    , _packageDTOLicense = pkg ^. license
    , _packageDTOPreviousPackageId = pkg ^. previousPackageId
    , _packageDTOForkOfPackageId = pkg ^. forkOfPackageId
    , _packageDTOMergeCheckpointPackageId = pkg ^. mergeCheckpointPackageId
    , _packageDTOEvents = pkg ^. events
    , _packageDTOCreatedAt = pkg ^. createdAt
    }

toSuggestionDTO :: Package -> Page PackageGroup -> PackageSuggestionDTO
toSuggestionDTO pkg groups =
  PackageSuggestionDTO
    { _packageSuggestionDTOPId = pkg ^. pId
    , _packageSuggestionDTOName = pkg ^. name
    , _packageSuggestionDTOVersion = pkg ^. version
    , _packageSuggestionDTODescription = pkg ^. description
    , _packageSuggestionDTOVersions = vs
    }
  where
    vs =
      case L.find (\g -> g ^. organizationId == pkg ^. organizationId && g ^. kmId == pkg ^. kmId) groups of
        Just group -> L.sort $ splitOn "," (group ^. versions)
        Nothing -> []

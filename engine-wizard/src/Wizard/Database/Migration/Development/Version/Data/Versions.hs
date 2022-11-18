module Wizard.Database.Migration.Development.Version.Data.Versions where

import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Package.PackageWithEvents
import Wizard.Api.Resource.Version.VersionDTO

versionAmsterdam :: VersionDTO
versionAmsterdam =
  VersionDTO
    { description = amsterdamPackage.description
    , readme = amsterdamPackage.readme
    , license = amsterdamPackage.license
    }

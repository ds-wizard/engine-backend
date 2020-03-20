module Wizard.Database.Migration.Development.Version.Data.Versions where

import Control.Lens ((^.))

import LensesConfig
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Api.Resource.Version.VersionDTO

versionAmsterdam :: VersionDTO
versionAmsterdam =
  VersionDTO
    { _versionDTODescription = amsterdamPackage ^. description
    , _versionDTOReadme = amsterdamPackage ^. readme
    , _versionDTOLicense = amsterdamPackage ^. license
    }

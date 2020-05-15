module Wizard.Service.Template.TemplateMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Package.Package
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Model.Template.Template
import Wizard.Service.Package.PackageMapper

toDTO :: [Package] -> Template -> TemplateDTO
toDTO pkgs template =
  TemplateDTO
    { _templateDTOUuid = template ^. uuid
    , _templateDTOName = template ^. name
    , _templateDTODescription = template ^. description
    , _templateDTOAllowedPackages = fmap toSimpleDTO pkgs
    , _templateDTORecommendedPackageId = template ^. recommendedPackageId
    , _templateDTOFormats = template ^. formats
    }

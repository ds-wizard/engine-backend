module Registry.Api.Resource.DocumentTemplate.Bundle.DocumentTemplateBundleJM where

import Data.Aeson

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Common.Api.Resource.Common.SemVer2TupleJM ()
import Shared.Common.Util.Aeson
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import WizardLib.KnowledgeModel.Model.Package.PackagePattern

instance ToJSON DocumentTemplateBundleDTO where
  toJSON DocumentTemplateBundleDTO {..} =
    object
      [ "id" .= tId
      , "name" .= name
      , "organizationId" .= organizationId
      , "templateId" .= templateId
      , "version" .= version
      , "metamodelVersion" .= metamodelVersion
      , "description" .= description
      , "readme" .= readme
      , "license" .= license
      , "allowedPackages" .= allowedPackages
      , "recommendedPackageId" .= Null
      , "formats" .= formats
      , "files" .= files
      , "assets" .= assets
      , "createdAt" .= createdAt
      ]

instance ToJSON PackagePattern where
  toJSON = genericToJSON jsonOptions

module Registry.Api.Resource.DocumentTemplate.Bundle.DocumentTemplateBundleJM where

import Data.Aeson

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import Shared.Model.Package.PackagePattern
import Shared.Util.Aeson

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

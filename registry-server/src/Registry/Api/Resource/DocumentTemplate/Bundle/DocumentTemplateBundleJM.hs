module Registry.Api.Resource.DocumentTemplate.Bundle.DocumentTemplateBundleJM where

import Data.Aeson

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Common.Api.Resource.Common.SemVer2TupleJM ()
import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

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

instance ToJSON KnowledgeModelPackagePattern where
  toJSON = genericToJSON jsonOptions

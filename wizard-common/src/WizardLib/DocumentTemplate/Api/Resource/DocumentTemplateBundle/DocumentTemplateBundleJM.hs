module WizardLib.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleJM where

import Control.Monad
import Data.Aeson

import Shared.Common.Api.Resource.Common.SemVer2TupleJM ()
import Shared.Common.Util.Aeson
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

instance ToJSON DocumentTemplateBundleDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateBundleDTO where
  parseJSON (Object o) = do
    tId <- o .: "id"
    name <- o .: "name"
    organizationId <- o .: "organizationId"
    templateId <- o .: "templateId"
    version <- o .: "version"
    metamodelVersion <- o .: "metamodelVersion"
    description <- o .: "description"
    readme <- o .: "readme"
    license <- o .: "license"
    allowedPackages <- o .: "allowedPackages"
    formats <- o .: "formats"
    files <- o .: "files"
    assets <- o .: "assets"
    createdAt <- o .: "createdAt"
    return DocumentTemplateBundleDTO {..}
  parseJSON _ = mzero

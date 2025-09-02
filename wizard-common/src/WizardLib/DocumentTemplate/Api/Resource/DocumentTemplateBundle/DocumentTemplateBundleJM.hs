module WizardLib.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleJM where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BSL

import Shared.Common.Api.Resource.Common.SemVer2TupleJM ()
import Shared.Common.Api.Resource.Localization.LocaleRecordJM ()
import Shared.Common.Model.Common.SemVer2Tuple
import Shared.Common.Util.Aeson
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import WizardLib.DocumentTemplate.Constant.DocumentTemplate
import WizardLib.DocumentTemplate.Localization.Messages.Public
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
    metamodelVersion <-
      (o .: "metamodelVersion" :: Parser SemVer2Tuple)
        <|> fail (BSL.unpack . encode $ _ERROR_VALIDATION__TEMPLATE_UNSUPPORTED_METAMODEL_VERSION tId "<<unable-to-parse>>" (show documentTemplateMetamodelVersion))
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

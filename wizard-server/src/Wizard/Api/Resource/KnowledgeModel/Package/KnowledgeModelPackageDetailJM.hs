module Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Wizard.Api.Resource.Registry.RegistryOrganizationJM ()

instance FromJSON KnowledgeModelPackageDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageDetailDTO where
  toJSON = genericToJSON jsonOptions

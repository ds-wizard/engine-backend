module Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageSimple
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Api.Resource.Registry.RegistryOrganizationJM ()

instance FromJSON KnowledgeModelPackageSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageSimpleDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelPackageSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageSimple where
  toJSON = genericToJSON jsonOptions

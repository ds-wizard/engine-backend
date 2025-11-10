module Wizard.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretJM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets
import Wizard.Model.KnowledgeModel.KnowledgeModelSecret

instance ToSchema KnowledgeModelSecret where
  declareNamedSchema = toSwagger kmSecret1

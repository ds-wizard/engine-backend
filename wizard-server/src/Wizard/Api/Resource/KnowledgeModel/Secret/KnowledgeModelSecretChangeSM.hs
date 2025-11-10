module Wizard.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO
import Wizard.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeJM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets

instance ToSchema KnowledgeModelSecretChangeDTO where
  declareNamedSchema = toSwagger kmSecret1ChangeDTO

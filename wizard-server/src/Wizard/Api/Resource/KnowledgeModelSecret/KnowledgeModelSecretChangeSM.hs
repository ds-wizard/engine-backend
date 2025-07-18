module Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretChangeDTO
import Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretChangeJM ()
import Wizard.Database.Migration.Development.KnowledgeModelSecret.Data.KnowledgeModelSecrets

instance ToSchema KnowledgeModelSecretChangeDTO where
  declareNamedSchema = toSwagger kmSecret1ChangeDTO

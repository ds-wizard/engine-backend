module Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretJM ()
import Wizard.Database.Migration.Development.KnowledgeModelSecret.Data.KnowledgeModelSecrets
import Wizard.Model.KnowledgeModelSecret.KnowledgeModelSecret

instance ToSchema KnowledgeModelSecret where
  declareNamedSchema = toSwagger kmSecret1

module Wizard.Api.Resource.Document.DocumentCreateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentCreateJM ()
import Wizard.Database.Migration.Development.Document.Data.Documents

instance ToSchema DocumentCreateDTO where
  declareNamedSchema = simpleToSchema doc1Create

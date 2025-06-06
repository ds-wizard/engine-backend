module WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileJM ()

instance ToSchema TemporaryFileDTO where
  declareNamedSchema = toSwagger $ TemporaryFileDTO {url = "http://example.com/temporary-file-1", contentType = "text/plain"}

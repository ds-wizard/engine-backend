module Wizard.Api.Resource.BookReference.BookReferenceSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.BookReference.BookReferenceDTO
import Wizard.Api.Resource.BookReference.BookReferenceJM ()
import Wizard.Database.Migration.Development.BookReference.Data.BookReferences
import Wizard.Service.BookReference.BookReferenceMapper

instance ToSchema BookReferenceDTO where
  declareNamedSchema = simpleToSchema (toDTO bookReferenceBvq)

module Wizard.Api.Resource.BookReference.BookReferenceSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.BookReference.BookReferenceJM ()
import Wizard.Database.Migration.Development.BookReference.Data.BookReferences
import Wizard.Model.BookReference.BookReference

instance ToSchema BookReference where
  declareNamedSchema = simpleToSchema' "_bookReference" bookReferenceBvq

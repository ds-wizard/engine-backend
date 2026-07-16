module Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListJM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Locale.KnowledgeModelLocales
import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList

instance ToSchema KnowledgeModelLocaleList where
  declareNamedSchema = toSwagger czechGlobalKmLocaleList

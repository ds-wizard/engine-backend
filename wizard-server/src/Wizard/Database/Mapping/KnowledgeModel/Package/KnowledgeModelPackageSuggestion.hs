module Wizard.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageSuggestion where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion

instance FromRow KnowledgeModelPackageSuggestion where
  fromRow = do
    pId <- field
    name <- field
    version <- field
    description <- field
    return $ KnowledgeModelPackageSuggestion {..}

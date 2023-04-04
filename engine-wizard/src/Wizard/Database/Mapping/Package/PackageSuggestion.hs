module Wizard.Database.Mapping.Package.PackageSuggestion where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Wizard.Model.Package.PackageSuggestion

instance FromRow PackageSuggestion where
  fromRow = do
    pId <- field
    name <- field
    version <- field
    description <- field
    return $ PackageSuggestion {..}

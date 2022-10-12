module Wizard.Database.Mapping.Package.PackageSuggestion where

import qualified Data.List as L
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Util.Coordinate
import Wizard.Model.Package.PackageSuggestion

instance FromRow PackageSuggestion where
  fromRow = do
    _packageSuggestionPId <- field
    _packageSuggestionName <- field
    _packageSuggestionVersion <- field
    _packageSuggestionDescription <- field
    versions <- fromPGArray <$> field
    let _packageSuggestionVersions = L.sortBy compareVersion versions
    return $ PackageSuggestion {..}

module Wizard.Database.Mapping.Package.PackageSuggestion where

import qualified Data.List as L
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Util.Coordinate
import Wizard.Model.Package.PackageSuggestion

instance FromRow PackageSuggestion where
  fromRow = do
    pId <- field
    name <- field
    version <- field
    description <- field
    versionsL <- fromPGArray <$> field
    let versions = L.sortBy compareVersion versionsL
    return $ PackageSuggestion {..}

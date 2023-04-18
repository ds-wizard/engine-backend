module Wizard.Database.Mapping.Package.PackageGroup where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import WizardLib.KnowledgeModel.Model.Package.PackageGroup

instance FromRow PackageGroup where
  fromRow = do
    organizationId <- field
    kmId <- field
    versions <- field
    return $ PackageGroup {..}

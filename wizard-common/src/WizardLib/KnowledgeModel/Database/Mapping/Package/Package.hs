module WizardLib.KnowledgeModel.Database.Mapping.Package.Package where

import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import WizardLib.KnowledgeModel.Database.Mapping.Package.PackagePhase ()
import WizardLib.KnowledgeModel.Model.Package.Package

instance FromRow Package where
  fromRow = do
    pId <- field
    name <- field
    organizationId <- field
    kmId <- field
    version <- field
    metamodelVersion <- field
    description <- field
    readme <- field
    license <- field
    previousPackageId <- field
    forkOfPackageId <- field
    mergeCheckpointPackageId <- field
    _ <- field :: RowParser Value
    createdAt <- field
    tenantUuid <- field
    phase <- field
    nonEditable <- field
    return $ Package {..}

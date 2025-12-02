module Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackage where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackagePhase ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

instance ToRow KnowledgeModelPackage where
  toRow KnowledgeModelPackage {..} =
    [ toField pId
    , toField name
    , toField organizationId
    , toField kmId
    , toField version
    , toField metamodelVersion
    , toField description
    , toField readme
    , toField license
    , toField previousPackageId
    , toField forkOfPackageId
    , toField mergeCheckpointPackageId
    , toField createdAt
    , toField tenantUuid
    , toField phase
    , toField nonEditable
    ]

instance FromRow KnowledgeModelPackage where
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
    createdAt <- field
    tenantUuid <- field
    phase <- field
    nonEditable <- field
    return $ KnowledgeModelPackage {..}

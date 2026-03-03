module Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackage where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Coordinate.Database.Mapping.Coordinate.Coordinate ()
import Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackagePhase ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

instance ToRow KnowledgeModelPackage where
  toRow KnowledgeModelPackage {..} =
    [ toField uuid
    , toField name
    , toField organizationId
    , toField kmId
    , toField version
    , toField metamodelVersion
    , toField description
    , toField readme
    , toField license
    , toField previousPackageUuid
    , toField forkOfPackageId
    , toField mergeCheckpointPackageId
    , toField createdAt
    , toField tenantUuid
    , toField phase
    , toField nonEditable
    , toField public
    ]

instance FromRow KnowledgeModelPackage where
  fromRow = do
    uuid <- field
    name <- field
    organizationId <- field
    kmId <- field
    version <- field
    metamodelVersion <- field
    description <- field
    readme <- field
    license <- field
    previousPackageUuid <- field
    forkOfPackageId <- field
    mergeCheckpointPackageId <- field
    createdAt <- field
    tenantUuid <- field
    phase <- field
    nonEditable <- field
    public <- field
    return $ KnowledgeModelPackage {..}

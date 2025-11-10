module Registry.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageRaw where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Registry.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw
import Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackagePhase ()

instance FromRow KnowledgeModelPackageRaw where
  fromRow = do
    pId <- field
    name <- field
    organizationId <- field
    kmId <- field
    version <- field
    phase <- field
    metamodelVersion <- field
    description <- field
    readme <- field
    license <- field
    previousPackageId <- field
    forkOfPackageId <- field
    mergeCheckpointPackageId <- field
    events <- fieldWith fromJSONField
    nonEditable <- field
    createdAt <- field
    return $ KnowledgeModelPackageRaw {..}

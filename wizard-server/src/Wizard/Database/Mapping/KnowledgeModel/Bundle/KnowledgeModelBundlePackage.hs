module Wizard.Database.Mapping.KnowledgeModel.Bundle.KnowledgeModelBundlePackage where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackagePhase ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage

instance ToRow KnowledgeModelBundlePackage where
  toRow KnowledgeModelBundlePackage {..} =
    [ toField pId
    , toField name
    , toField organizationId
    , toField kmId
    , toField version
    , toField phase
    , toField metamodelVersion
    , toField description
    , toField readme
    , toField license
    , toField previousPackageId
    , toField forkOfPackageId
    , toField mergeCheckpointPackageId
    , toJSONField events
    , toField nonEditable
    , toField createdAt
    ]

instance FromRow KnowledgeModelBundlePackage where
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
    return $ KnowledgeModelBundlePackage {..}

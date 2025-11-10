module Wizard.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditor where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor

instance FromRow KnowledgeModelEditor where
  fromRow = do
    uuid <- field
    name <- field
    kmId <- field
    previousPackageId <- field
    createdBy <- field
    createdAt <- field
    updatedAt <- field
    tenantUuid <- field
    version <- field
    description <- field
    readme <- field
    license <- field
    metamodelVersion <- field
    squashed <- field
    return $ KnowledgeModelEditor {..}

instance ToRow KnowledgeModelEditor where
  toRow KnowledgeModelEditor {..} =
    [ toField uuid
    , toField name
    , toField kmId
    , toField previousPackageId
    , toField createdBy
    , toField createdAt
    , toField updatedAt
    , toField tenantUuid
    , toField version
    , toField description
    , toField readme
    , toField license
    , toField metamodelVersion
    , toField squashed
    ]

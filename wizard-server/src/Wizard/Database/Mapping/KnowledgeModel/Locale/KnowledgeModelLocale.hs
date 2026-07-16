module Wizard.Database.Mapping.KnowledgeModel.Locale.KnowledgeModelLocale where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocale

instance ToRow KnowledgeModelLocale where
  toRow KnowledgeModelLocale {..} =
    [ toField uuid
    , toField name
    , toField code
    , toField knowledgeModelPackageUuid
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow KnowledgeModelLocale where
  fromRow = do
    uuid <- field
    name <- field
    code <- field
    knowledgeModelPackageUuid <- field
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    return $ KnowledgeModelLocale {..}

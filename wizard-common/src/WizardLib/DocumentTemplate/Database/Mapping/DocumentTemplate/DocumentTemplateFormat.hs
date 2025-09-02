module WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateFormat where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

instance ToRow DocumentTemplateFormat where
  toRow DocumentTemplateFormat {..} =
    [ toField documentTemplateId
    , toField uuid
    , toField name
    , toField icon
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow DocumentTemplateFormat where
  fromRow = do
    documentTemplateId <- field
    uuid <- field
    name <- field
    icon <- field
    let steps = []
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    return $ DocumentTemplateFormat {..}

instance ToRow DocumentTemplateFormatStep where
  toRow DocumentTemplateFormatStep {..} =
    [ toField documentTemplateId
    , toField formatUuid
    , toField position
    , toField name
    , toJSONField options
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow DocumentTemplateFormatStep where
  fromRow = do
    documentTemplateId <- field
    formatUuid <- field
    position <- field
    name <- field
    options <- fieldWith fromJSONField
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    return $ DocumentTemplateFormatStep {..}

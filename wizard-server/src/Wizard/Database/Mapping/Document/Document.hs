module Wizard.Database.Mapping.Document.Document where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Common.Database.Mapping.Common
import Wizard.Model.Document.Document

instance ToField DocumentDurability where
  toField = toFieldGenericEnum

instance FromField DocumentDurability where
  fromField = fromFieldGenericEnum

instance ToField DocumentState where
  toField = toFieldGenericEnum

instance FromField DocumentState where
  fromField = fromFieldGenericEnum

instance ToRow Document where
  toRow Document {..} =
    [ toField uuid
    , toField name
    , toField state
    , toField durability
    , toField questionnaireUuid
    , toField questionnaireEventUuid
    , toField questionnaireRepliesHash
    , toField documentTemplateId
    , toField formatUuid
    , toField creatorUuid
    , toField retrievedAt
    , toField finishedAt
    , toField createdAt
    , toField fileName
    , toField contentType
    , toField workerLog
    , toField appUuid
    , toField fileSize
    ]

instance FromRow Document where
  fromRow = do
    uuid <- field
    name <- field
    state <- field
    durability <- field
    questionnaireUuid <- field
    questionnaireEventUuid <- field
    questionnaireRepliesHash <- field
    documentTemplateId <- field
    formatUuid <- field
    creatorUuid <- field
    retrievedAt <- field
    finishedAt <- field
    createdAt <- field
    fileName <- field
    contentType <- field
    workerLog <- field
    appUuid <- field
    fileSize <- field
    return $ Document {..}

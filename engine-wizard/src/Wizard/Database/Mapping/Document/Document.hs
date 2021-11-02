module Wizard.Database.Mapping.Document.Document where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Database.Mapping.Common
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
    [ toField _documentUuid
    , toField _documentName
    , toField _documentState
    , toField _documentDurability
    , toField _documentQuestionnaireUuid
    , toField _documentQuestionnaireEventUuid
    , toField _documentQuestionnaireRepliesHash
    , toField _documentTemplateId
    , toField _documentFormatUuid
    , toField _documentCreatorUuid
    , toField _documentRetrievedAt
    , toField _documentFinishedAt
    , toField _documentCreatedAt
    , toField _documentFileName
    , toField _documentContentType
    , toField _documentWorkerLog
    , toField _documentAppUuid
    ]

instance FromRow Document where
  fromRow = do
    _documentUuid <- field
    _documentName <- field
    _documentState <- field
    _documentDurability <- field
    _documentQuestionnaireUuid <- field
    _documentQuestionnaireEventUuid <- field
    _documentQuestionnaireRepliesHash <- field
    _documentTemplateId <- field
    _documentFormatUuid <- field
    _documentCreatorUuid <- field
    _documentRetrievedAt <- field
    _documentFinishedAt <- field
    _documentCreatedAt <- field
    _documentFileName <- field
    _documentContentType <- field
    _documentWorkerLog <- field
    _documentAppUuid <- field
    return $ Document {..}

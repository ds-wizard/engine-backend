module Wizard.Database.BSON.Document.Document where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Wizard.Database.BSON.Common ()
import Wizard.Database.BSON.Document.DocumentType ()
import Wizard.Model.Document.Document

instance ToBSON DocumentState

instance FromBSON DocumentState

instance ToBSON DocumentMetadata where
  toBSON DocumentMetadata {..} =
    ["fileName" BSON.=: _documentMetadataFileName, "contentType" BSON.=: _documentMetadataContentType]

instance FromBSON DocumentMetadata where
  fromBSON doc = do
    _documentMetadataFileName <- BSON.lookup "fileName" doc
    _documentMetadataContentType <- BSON.lookup "contentType" doc
    return DocumentMetadata {..}

instance ToBSON Document where
  toBSON Document {..} =
    [ "uuid" BSON.=: _documentUuid
    , "name" BSON.=: _documentName
    , "state" BSON.=: _documentState
    , "questionnaireUuid" BSON.=: _documentQuestionnaireUuid
    , "templateUuid" BSON.=: _documentTemplateUuid
    , "formatUuid" BSON.=: _documentFormatUuid
    , "metadata" BSON.=: _documentMetadata
    , "ownerUuid" BSON.=: _documentOwnerUuid
    , "createdAt" BSON.=: _documentCreatedAt
    ]

instance FromBSON Document where
  fromBSON doc = do
    _documentUuid <- BSON.lookup "uuid" doc
    _documentName <- BSON.lookup "name" doc
    _documentState <- BSON.lookup "state" doc
    _documentQuestionnaireUuid <- BSON.lookup "questionnaireUuid" doc
    _documentTemplateUuid <- BSON.lookup "templateUuid" doc
    _documentFormatUuid <- BSON.lookup "formatUuid" doc
    _documentMetadata <- BSON.lookup "metadata" doc
    _documentOwnerUuid <- BSON.lookup "ownerUuid" doc
    _documentCreatedAt <- BSON.lookup "createdAt" doc
    return Document {..}

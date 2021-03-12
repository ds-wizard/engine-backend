module Wizard.Model.Document.Document where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data DocumentState
  = QueuedDocumentState
  | InProgressDocumentState
  | DoneDocumentState
  | ErrorDocumentState
  deriving (Show, Eq, Generic, Read)

data DocumentMetadata =
  DocumentMetadata
    { _documentMetadataFileName :: Maybe String
    , _documentMetadataContentType :: Maybe String
    }
  deriving (Show, Eq, Generic)

data DocumentDurability
  = PersistentDocumentDurability
  | TemporallyDocumentDurability
  deriving (Show, Eq, Generic, Read)

data Document =
  Document
    { _documentUuid :: U.UUID
    , _documentName :: String
    , _documentState :: DocumentState
    , _documentDurability :: DocumentDurability
    , _documentQuestionnaireUuid :: U.UUID
    , _documentQuestionnaireEventUuid :: Maybe U.UUID
    , _documentQuestionnaireRepliesHash :: Int
    , _documentTemplateId :: String
    , _documentFormatUuid :: U.UUID
    , _documentMetadata :: DocumentMetadata
    , _documentCreatorUuid :: Maybe U.UUID
    , _documentCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

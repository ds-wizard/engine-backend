module Wizard.Model.Document.Document where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

data DocumentState
  = QueuedDocumentState
  | InProgressDocumentState
  | DoneDocumentState
  | ErrorDocumentState
  deriving (Show, Eq, Generic, Read)

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
    , _documentCreatorUuid :: Maybe U.UUID
    , _documentFileName :: Maybe String
    , _documentContentType :: Maybe String
    , _documentFileSize :: Maybe Int64
    , _documentWorkerLog :: Maybe String
    , _documentAppUuid :: U.UUID
    , _documentRetrievedAt :: Maybe UTCTime
    , _documentFinishedAt :: Maybe UTCTime
    , _documentCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

module Wizard.Model.Document.Document where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data DocumentState
  = QueuedDocumentState
  | InProgressDocumentState
  | DoneDocumentState
  | ErrorDocumentState
  deriving (Show, Eq, Generic)

data DocumentMetadata =
  DocumentMetadata
    { _documentMetadataFileName :: Maybe String
    , _documentMetadataContentType :: Maybe String
    }
  deriving (Show, Eq, Generic)

data Document =
  Document
    { _documentUuid :: U.UUID
    , _documentName :: String
    , _documentState :: DocumentState
    , _documentQuestionnaireUuid :: U.UUID
    , _documentTemplateUuid :: U.UUID
    , _documentFormatUuid :: U.UUID
    , _documentMetadata :: DocumentMetadata
    , _documentOwnerUuid :: U.UUID
    , _documentCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

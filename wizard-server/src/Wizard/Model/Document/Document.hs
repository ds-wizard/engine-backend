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

data Document = Document
  { uuid :: U.UUID
  , name :: String
  , state :: DocumentState
  , durability :: DocumentDurability
  , questionnaireUuid :: U.UUID
  , questionnaireEventUuid :: Maybe U.UUID
  , questionnaireRepliesHash :: Int
  , documentTemplateId :: String
  , formatUuid :: U.UUID
  , creatorUuid :: Maybe U.UUID
  , fileName :: Maybe String
  , contentType :: Maybe String
  , fileSize :: Maybe Int64
  , workerLog :: Maybe String
  , tenantUuid :: U.UUID
  , retrievedAt :: Maybe UTCTime
  , finishedAt :: Maybe UTCTime
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

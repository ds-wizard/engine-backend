module Wizard.Model.Document.DocumentQueue where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Document.DocumentContext

data DocumentQueue =
  DocumentQueue
    { _documentQueueDId :: Int
    , _documentQueueDocumentUuid :: U.UUID
    , _documentQueueDocumentContext :: DocumentContext
    , _documentQueueAppUuid :: U.UUID
    , _documentQueueCreatedBy :: Maybe U.UUID
    , _documentQueueCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

module Wizard.Database.Mapping.Document.DocumentQueue where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Document.DocumentContextJM ()
import Wizard.Model.Document.DocumentQueue

instance ToRow DocumentQueue where
  toRow DocumentQueue {..} =
    [ toField _documentQueueDocumentUuid
    , toJSONField _documentQueueDocumentContext
    , toField _documentQueueCreatedBy
    , toField _documentQueueCreatedAt
    , toField _documentQueueAppUuid
    ]

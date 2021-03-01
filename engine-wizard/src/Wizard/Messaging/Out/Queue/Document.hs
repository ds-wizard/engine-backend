module Wizard.Messaging.Out.Queue.Document where

import Control.Lens ((^.))
import Data.Aeson (encode)

import LensesConfig
import Wizard.Api.Resource.Document.DocumentContextJM ()
import Wizard.Messaging.Out.Queue.Common
import Wizard.Messaging.Resource.Document.DocumentCreateMDTO
import Wizard.Messaging.Resource.Document.DocumentCreateMJM ()
import Wizard.Messaging.Route
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Service.Document.DocumentContextService

publishToDocumentQueue :: Document -> AppContextM ()
publishToDocumentQueue doc = do
  docContext <- createDocumentContext doc
  let dto =
        DocumentCreateMDTO
          {_documentCreateMDTODocumentUuid = doc ^. uuid, _documentCreateMDTODocumentContext = docContext}
  publishToQueue _DOCUMENT_QUEUE (encode dto)
  return ()

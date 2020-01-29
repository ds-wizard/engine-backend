module Wizard.Messaging.Out.Queue.Document where

import Control.Lens ((^.))
import Data.Aeson (encode)
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Error.Error
import Shared.Util.Helper (createHeeHelper)
import Wizard.Api.Resource.Document.DocumentContextJM ()
import Wizard.Messaging.Out.Queue.Common
import Wizard.Messaging.Resource.Document.DocumentCreateMDTO
import Wizard.Messaging.Resource.Document.DocumentCreateMJM ()
import Wizard.Messaging.Route
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Service.Document.DocumentContextService

publishToDocumentQueue :: Document -> AppContextM (Either AppError ())
publishToDocumentQueue doc =
  heCreateDocumentContext (U.toString $ doc ^. questionnaireUuid) $ \docContext -> do
    let dto =
          DocumentCreateMDTO
            {_documentCreateMDTODocumentUuid = doc ^. uuid, _documentCreateMDTODocumentContext = docContext}
    publishToQueue _DOCUMENT_QUEUE (encode dto)
    return . Right $ ()

-- --------------------------------
-- HELPERS
-- --------------------------------
hePublishToDocumentQueue doc = createHeeHelper (publishToDocumentQueue doc)

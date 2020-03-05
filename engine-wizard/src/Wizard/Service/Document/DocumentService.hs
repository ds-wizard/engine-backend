module Wizard.Service.Document.DocumentService where

import Control.Lens ((^.))
import Control.Monad (forM)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.Uuid
import Wizard.Api.Resource.Document.DocumentContextJM ()
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Messaging.Out.Queue.Document
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Document.Document
import Wizard.Service.Document.DocumentMapper
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Template.TemplateService

getDocumentsFiltered :: [(String, String)] -> AppContextM [DocumentDTO]
getDocumentsFiltered queryParams = do
  currentUser <- getCurrentUser
  documents <- _getDocuments currentUser
  forM documents enhance
  where
    _getDocuments currentUser =
      if currentUser ^. role == "ADMIN"
        then findDocumentsFiltered queryParams
        else findDocumentsFiltered (queryParams ++ [("ownerUuid", U.toString $ currentUser ^. uuid)])
    enhance :: Document -> AppContextM DocumentDTO
    enhance doc = do
      tml <- getTemplateByUuid (U.toString $ doc ^. templateUuid) Nothing
      mQtn <- catchError (getQuestionnaireById' (U.toString $ doc ^. questionnaireUuid)) (\_ -> return Nothing)
      return $ toDTO doc mQtn tml

createDocument :: DocumentCreateDTO -> AppContextM DocumentDTO
createDocument dto = do
  qtn <- getQuestionnaireById (U.toString $ dto ^. questionnaireUuid)
  tml <- getTemplateByUuid (U.toString $ dto ^. templateUuid) (Just $ qtn ^. package . pId)
  currentUser <- getCurrentUser
  dUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let doc = fromCreateDTO dto dUuid (currentUser ^. uuid) now
  insertDocument doc
  publishToDocumentQueue doc
  return $ toDTO doc (Just qtn) tml

deleteDocument :: String -> AppContextM ()
deleteDocument docUuid = do
  currentUser <- getCurrentUser
  doc <- findDocumentById docUuid
  if currentUser ^. role == "ADMIN" || currentUser ^. uuid == doc ^. ownerUuid
    then do
      deleteDocumentById docUuid
      deleteDocumentContentById docUuid
      return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Delete Document"

downloadDocument :: String -> AppContextM (Document, BS.ByteString)
downloadDocument docUuid = do
  doc <- findDocumentById docUuid
  content <- findDocumentContent docUuid
  return (doc, content)

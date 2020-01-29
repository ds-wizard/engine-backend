module Wizard.Service.Document.DocumentService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
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
import Wizard.Util.List (foldEithersInContext)

getDocumentsFiltered :: [(T.Text, T.Text)] -> AppContextM (Either AppError [DocumentDTO])
getDocumentsFiltered queryParams =
  heGetCurrentUser $ \currentUser ->
    _heGetDocuments currentUser $ \documents -> foldEithersInContext . fmap toDTO' $ documents
  where
    _heGetDocuments currentUser =
      if currentUser ^. role == "ADMIN"
        then heFindDocumentsFiltered queryParams
        else heFindDocumentsFiltered (queryParams ++ [("ownerUuid", U.toText $ currentUser ^. uuid)])
    toDTO' :: Document -> AppContextM (Either AppError DocumentDTO)
    toDTO' doc =
      heGetTemplateByUuid (U.toString $ doc ^. templateUuid) Nothing $ \tml -> do
        eQtn <- getQuestionnaireById (U.toString $ doc ^. questionnaireUuid)
        case eQtn of
          Right qtn -> return . Right $ toDTO doc (Just qtn) tml
          Left _ -> return . Right $ toDTO doc Nothing tml

createDocument :: DocumentCreateDTO -> AppContextM (Either AppError DocumentDTO)
createDocument dto =
  heGetQuestionnaireById (U.toString $ dto ^. questionnaireUuid) $ \qtn ->
    heGetTemplateByUuid (U.toString $ dto ^. templateUuid) (Just $ qtn ^. package . pId) $ \tml ->
      heGetCurrentUser $ \currentUser -> do
        dUuid <- liftIO generateUuid
        now <- liftIO getCurrentTime
        let doc = fromCreateDTO dto dUuid (currentUser ^. uuid) now
        insertDocument doc
        hePublishToDocumentQueue doc $ \_ -> return . Right $ toDTO doc (Just qtn) tml

deleteDocument :: String -> AppContextM (Maybe AppError)
deleteDocument docUuid =
  hmGetCurrentUser $ \currentUser ->
    hmFindDocumentById docUuid $ \doc ->
      if currentUser ^. role == "ADMIN" || currentUser ^. uuid == doc ^. ownerUuid
        then do
          deleteDocumentById docUuid
          deleteDocumentContentById docUuid
          return Nothing
        else return . Just . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Delete Document"

downloadDocument :: String -> AppContextM (Either AppError (Document, BS.ByteString))
downloadDocument docUuid =
  heFindDocumentById docUuid $ \doc -> heFindDocumentContent docUuid $ \content -> return . Right $ (doc, content)

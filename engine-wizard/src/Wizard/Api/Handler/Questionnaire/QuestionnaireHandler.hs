module Wizard.Api.Handler.Questionnaire.QuestionnaireHandler where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types.Status (created201, noContent204)
import Text.Read (readMaybe)
import Web.Scotty.Trans (addHeader, json, param, raw, status)

import Shared.Model.Error.Error
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Localization.Messages.Public
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Document.DocumentHelpers
import Wizard.Service.Document.DocumentService
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Report.ReportService

getQuestionnairesA :: Endpoint
getQuestionnairesA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    eitherDtos <- runInAuthService $ getQuestionnairesForCurrentUser
    case eitherDtos of
      Right dtos -> json dtos
      Left error -> sendError error

postQuestionnairesA :: Endpoint
postQuestionnairesA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      eitherQuestionnaireDto <- runInAuthService $ createQuestionnaire reqDto
      case eitherQuestionnaireDto of
        Left appError -> sendError appError
        Right questionnaireDto -> do
          status created201
          json questionnaireDto

getQuestionnaireA :: Endpoint
getQuestionnaireA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    qtnUuid <- param "qtnUuid"
    eitherDto <- runInAuthService $ getQuestionnaireDetailById qtnUuid
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error

putQuestionnaireA :: Endpoint
putQuestionnaireA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      qtnUuid <- param "qtnUuid"
      eitherDto <- runInAuthService $ modifyQuestionnaire qtnUuid reqDto
      case eitherDto of
        Right dto -> json dto
        Left error -> sendError error

getQuestionnaireDmpA :: Endpoint
getQuestionnaireDmpA = do
  qtnUuid <- param "qtnUuid"
  mFormatS <- getQueryParam "format"
  mTemplateUuid <- getQueryParam "templateUuid"
  case mFormatS of
    Nothing -> do
      eitherDto <- runInUnauthService $ createDocumentContext qtnUuid
      case eitherDto of
        Right dto -> json dto
        Left error -> sendError error
    Just "html-preview" -> do
      eitherHTMLto <- runInUnauthService $ exportDocument qtnUuid (T.unpack <$> mTemplateUuid) HTML
      case eitherHTMLto of
        Right html -> do
          addHeader "Content-Type" (LT.pack "text/html; charset=utf-8")
          raw $ html
        Left error -> sendError error
    Just formatS ->
      heGetFormat formatS $ \format -> do
        eitherBody <- runInUnauthService $ exportDocument qtnUuid (T.unpack <$> mTemplateUuid) format
        case eitherBody of
          Right body -> sendFile (getFilename qtnUuid format) body
          Left error -> sendError error
  where
    heGetFormat format callback =
      case readMaybe (T.unpack format) of
        Just knownFormat -> callback knownFormat
        Nothing -> sendError . UserError . _ERROR_VALIDATION__UNSUPPORTED_DMP_FORMAT $ T.unpack format
    getFilename :: String -> DocumentFormat -> String
    getFilename qtnUuid format = qtnUuid ++ formatExtension format

getQuestionnaireReportA :: Endpoint
getQuestionnaireReportA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    qtnUuid <- param "qtnUuid"
    eitherDto <- runInAuthService $ getReportByQuestionnaireUuid qtnUuid
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error

postQuestionnaireReportPreviewA :: Endpoint
postQuestionnaireReportPreviewA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      qtnUuid <- param "qtnUuid"
      eitherDto <- runInAuthService $ getPreviewOfReportByQuestionnaireUuid qtnUuid reqDto
      case eitherDto of
        Right dto -> json dto
        Left error -> sendError error

deleteQuestionnaireA :: Endpoint
deleteQuestionnaireA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    qtnUuid <- param "qtnUuid"
    maybeError <- runInAuthService $ deleteQuestionnaire qtnUuid
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error

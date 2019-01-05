module Api.Handler.Questionnaire.QuestionnaireHandler where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types.Status (created201, noContent204)
import Text.Read (readMaybe)
import Web.Scotty.Trans (addHeader, json, param, raw, status)

import Api.Handler.Common
import Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelDTO ()
import Api.Resource.Questionnaire.QuestionnaireChangeJM ()
import Api.Resource.Questionnaire.QuestionnaireCreateDTO ()
import Api.Resource.Questionnaire.QuestionnaireDTO ()
import Api.Resource.Report.ReportJM ()
import Localization
import Model.DataManagementPlan.DataManagementPlan
import Model.DataManagementPlan.DataManagementPlanHelpers
import Model.Error.ErrorHelpers
import Service.DataManagementPlan.DataManagementPlanService
import Service.Questionnaire.QuestionnaireService
import Service.Report.ReportService

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
  case mFormatS of
    Nothing -> do
      eitherDto <- runInUnauthService $ createDataManagementPlan qtnUuid
      case eitherDto of
        Right dto -> json dto
        Left error -> sendError error
    Just "html-preview" -> do
      eitherHTMLto <- runInUnauthService $ exportDataManagementPlan qtnUuid HTML
      case eitherHTMLto of
        Right html -> do
          addHeader "Content-Type" (LT.pack "text/html; charset=utf-8")
          raw $ html
        Left error -> sendError error
    Just formatS ->
      heGetFormat formatS $ \format -> do
        eitherBody <- runInUnauthService $ exportDataManagementPlan qtnUuid format
        case eitherBody of
          Right body -> sendFile (getFilename qtnUuid format) body
          Left error -> sendError error
  where
    heGetFormat format callback =
      case readMaybe (T.unpack format) of
        Just knownFormat -> callback knownFormat
        Nothing -> sendError . createErrorWithErrorMessage . _ERROR_VALIDATION__UNSUPPORTED_DMP_FORMAT $ T.unpack format
    getFilename :: String -> DataManagementPlanFormat -> String
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

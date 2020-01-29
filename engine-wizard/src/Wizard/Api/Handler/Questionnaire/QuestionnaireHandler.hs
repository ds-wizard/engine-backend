module Wizard.Api.Handler.Questionnaire.QuestionnaireHandler where

import qualified Data.Text as T
import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Report.ReportService

getQuestionnairesA :: Endpoint
getQuestionnairesA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    eitherDtos <- runInAuthService getQuestionnairesForCurrentUser
    case eitherDtos of
      Right dtos -> json dtos
      Left error -> sendError error

postQuestionnairesA :: Endpoint
postQuestionnairesA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    mCloneUuid <- getQueryParam "cloneUuid"
    case mCloneUuid of
      Just cloneUuid -> do
        eitherQuestionnaireDto <- runInAuthService $ cloneQuestionnaire (T.unpack cloneUuid)
        case eitherQuestionnaireDto of
          Left appError -> sendError appError
          Right questionnaireDto -> do
            status created201
            json questionnaireDto
      Nothing ->
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

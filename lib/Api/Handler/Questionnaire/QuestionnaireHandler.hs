module Api.Handler.Questionnaire.QuestionnaireHandler where

import Control.Lens ((^.))
import Control.Monad.Reader (lift)
import Data.Aeson (encode)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.UUID as U
import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (addHeader, json, param, raw, status)

import Api.Handler.Common
import Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelDTO ()
import Api.Resource.Questionnaire.QuestionnaireCreateDTO ()
import Api.Resource.Questionnaire.QuestionnaireDTO ()
import Common.Error
import Common.Localization
import LensesConfig
import Service.DataManagementPlan.DataManagementPlanService
import Service.Questionnaire.QuestionnaireService

getQuestionnairesA :: Endpoint
getQuestionnairesA =
  checkPermission "QTN_PERM" $ do
    eitherDtos <- lift $ getQuestionnaires
    case eitherDtos of
      Right dtos -> json dtos
      Left error -> sendError error

postQuestionnairesA :: Endpoint
postQuestionnairesA =
  checkPermission "QTN_PERM" $
  getReqDto $ \reqDto -> do
    eitherQuestionnaireDto <- lift $ createQuestionnaire reqDto
    case eitherQuestionnaireDto of
      Left appError -> sendError appError
      Right questionnaireDto -> do
        status created201
        json questionnaireDto

getQuestionnaireA :: Endpoint
getQuestionnaireA =
  checkPermission "QTN_PERM" $ do
    qtnUuid <- param "qtnUuid"
    eitherDto <- lift $ getQuestionnaireDetailById qtnUuid
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error

putQuestionnaireRepliesA :: Endpoint
putQuestionnaireRepliesA =
  checkPermission "QTN_PERM" $
  getReqDto $ \reqDto -> do
    qtnUuid <- param "qtnUuid"
    eitherDto <- lift $ modifyQuestionnaireReplies qtnUuid reqDto
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error

getQuestionnaireDmpA :: Endpoint
getQuestionnaireDmpA = do
  qtnUuid <- param "qtnUuid"
  format <- getQueryParam "format"
  eitherDto <- lift $ createDataManagementPlan qtnUuid
  case eitherDto of
    Right dto -> do
      case format of
        Nothing -> json dto
        Just "json" -> do
          let cdHeader = "attachment;filename=" ++ (U.toString $ dto ^. uuid) ++ ".dmp"
          addHeader "Content-Disposition" (TL.pack cdHeader)
          addHeader "Content-Type" (TL.pack "application/octet-stream")
          raw $ encode dto
        Just unsupportedFormat ->
          sendError . createErrorWithErrorMessage . _ERROR_VALIDATION__UNSUPPORTED_DMP_FORMAT $
          (T.unpack unsupportedFormat)
    Left error -> sendError error

deleteQuestionnaireA :: Endpoint
deleteQuestionnaireA =
  checkPermission "QTN_PERM" $ do
    qtnUuid <- param "qtnUuid"
    maybeError <- lift $ deleteQuestionnaire qtnUuid
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error

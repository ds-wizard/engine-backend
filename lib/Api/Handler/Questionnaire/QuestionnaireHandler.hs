module Api.Handler.Questionnaire.QuestionnaireHandler where

import Control.Monad.Trans.Class (lift)
import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.Questionnaire.QuestionnaireCreateDTO ()
import Api.Resource.Questionnaire.QuestionnaireDTO ()
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
    eitherDto <- lift $ getQuestionnaireById qtnUuid
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

deleteQuestionnaireA :: Endpoint
deleteQuestionnaireA =
  checkPermission "QTN_PERM" $ do
    qtnUuid <- param "qtnUuid"
    maybeError <- lift $ deleteQuestionnaire qtnUuid
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error

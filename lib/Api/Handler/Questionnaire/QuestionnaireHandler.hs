module Api.Handler.Questionnaire.QuestionnaireHandler where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO, liftM)
import Control.Monad.Trans.Class (lift)
import Data.Text as T
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Api.Resource.Questionnaire.QuestionnaireDTO
import Common.Error
import Model.Context.AppContext
import Service.Questionnaire.QuestionnaireService

getQuestionnairesA :: Endpoint
getQuestionnairesA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "QTN_PERM" $ do
    eitherDtos <- liftIO $ getQuestionnaires context
    case eitherDtos of
      Right dtos -> json dtos
      Left error -> sendError error

postQuestionnairesA :: Endpoint
postQuestionnairesA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "QTN_PERM" $
    getReqDto $ \reqDto -> do
      eitherQuestionnaireDto <- liftIO $ createQuestionnaire context dswConfig reqDto
      case eitherQuestionnaireDto of
        Left appError -> sendError appError
        Right questionnaireDto -> do
          status created201
          json questionnaireDto

getQuestionnaireA :: Endpoint
getQuestionnaireA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "QTN_PERM" $ do
    qtnUuid <- param "qtnUuid"
    eitherDto <- liftIO $ getQuestionnaireById context qtnUuid
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error

putQuestionnaireRepliesA :: Endpoint
putQuestionnaireRepliesA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "QTN_PERM" $
    getReqDto $ \reqDto -> do
      qtnUuid <- param "qtnUuid"
      eitherDto <- liftIO $ modifyQuestionnaireReplies context qtnUuid reqDto
      case eitherDto of
        Right dto -> json dto
        Left error -> sendError error

deleteQuestionnaireA :: Endpoint
deleteQuestionnaireA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "QTN_PERM" $ do
    qtnUuid <- param "qtnUuid"
    maybeError <- liftIO $ deleteQuestionnaire context qtnUuid
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error

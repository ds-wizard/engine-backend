module Api.Handler.Feedback.FeedbackHandler where

import Control.Monad.Reader (lift)
import Network.HTTP.Types.Status (created201)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.Feedback.FeedbackCreateDTO ()
import Api.Resource.Feedback.FeedbackDTO ()
import Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelDTO ()
import Service.Feedback.FeedbackService

getFeedbacksA :: Endpoint
getFeedbacksA = do
  queryParams <- getListOfQueryParamsIfPresent ["packageId", "questionUuid"]
  eitherDtos <- lift $ getFeedbacksFiltered queryParams
  case eitherDtos of
    Right dtos -> json dtos
    Left error -> sendError error

postFeedbacksA :: Endpoint
postFeedbacksA = do
  getReqDto $ \reqDto -> do
    eitherFeedbackDto <- lift $ createFeedback reqDto
    case eitherFeedbackDto of
      Left appError -> sendError appError
      Right questionnaireDto -> do
        status created201
        json questionnaireDto

getFeedbackA :: Endpoint
getFeedbackA = do
  fUuid <- param "fUuid"
  eitherDto <- lift $ getFeedbackByUuid fUuid
  case eitherDto of
    Right dto -> json dto
    Left error -> sendError error

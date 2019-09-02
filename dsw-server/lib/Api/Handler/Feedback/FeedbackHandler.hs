module Api.Handler.Feedback.FeedbackHandler where

import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.Feedback.FeedbackCreateJM ()
import Api.Resource.Feedback.FeedbackJM ()
import Service.Feedback.FeedbackService

getFeedbacksA :: Endpoint
getFeedbacksA = do
  queryParams <- getListOfQueryParamsIfPresent ["packageId", "questionUuid"]
  eitherDtos <- runInUnauthService $ getFeedbacksFiltered queryParams
  case eitherDtos of
    Right dtos -> json dtos
    Left error -> sendError error

postFeedbacksA :: Endpoint
postFeedbacksA = do
  getReqDto $ \reqDto -> do
    eitherFeedbackDto <- runInUnauthService $ createFeedback reqDto
    case eitherFeedbackDto of
      Left appError -> sendError appError
      Right questionnaireDto -> do
        status created201
        json questionnaireDto

getFeedbacksSynchronizationA :: Endpoint
getFeedbacksSynchronizationA =
  checkServiceToken $ do
    maybeError <- runInUnauthService $ synchronizeFeedbacks
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error

getFeedbackA :: Endpoint
getFeedbackA = do
  fUuid <- param "fUuid"
  eitherDto <- runInUnauthService $ getFeedbackByUuid fUuid
  case eitherDto of
    Right dto -> json dto
    Left error -> sendError error

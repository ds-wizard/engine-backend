module Api.Handler.Metric.MetricHandler where

import Web.Scotty.Trans (json)

import Api.Handler.Common
import Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Service.Metric.MetricService

getMetricsA :: Endpoint
getMetricsA =
  getAuthServiceExecutor $ \runInAuthService -> do
    eitherDto <- runInAuthService getMetrics
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error

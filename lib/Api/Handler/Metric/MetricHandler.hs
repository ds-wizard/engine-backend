module Api.Handler.Metric.MetricHandler where

import Control.Monad.Trans.Class (lift)
import Web.Scotty.Trans (json)

import Api.Handler.Common
import Service.Metric.MetricService

getMetricsA :: Endpoint
getMetricsA = do
  eitherDto <- lift $ getMetrics
  case eitherDto of
    Right dto -> json dto
    Left error -> sendError error

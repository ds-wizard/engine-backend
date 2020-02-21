module Wizard.Api.Handler.Metric.List_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Metric.MetricService

type List_GET
   = Header "Authorization" String
     :> "metrics"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [MetricDTO])

list_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [MetricDTO])
list_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService -> runInAuthService $ addTraceUuidHeader =<< getMetrics

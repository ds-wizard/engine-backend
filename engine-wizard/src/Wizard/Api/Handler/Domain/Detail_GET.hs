module Wizard.Api.Handler.Domain.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Domain.DomainService

type Detail_GET
   = Header "Host" String
     :> "domains"
     :> Capture "appId" String
     :> Verb GET 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_GET :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_GET mServerUrl appId =
  runInUnauthService mServerUrl $
  addTraceUuidHeader =<< do
    getDomain appId
    return NoContent

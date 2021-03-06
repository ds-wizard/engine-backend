module Wizard.Api.Handler.Cache.List_DELETE where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Cache.CacheService

type List_DELETE
   = Header "Authorization" String
     :> "caches"
     :> Verb DELETE 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

list_DELETE :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
list_DELETE mServiceToken =
  runInUnauthService $
  addTraceUuidHeader =<< do
    checkServiceToken mServiceToken
    purgeCache
    return NoContent

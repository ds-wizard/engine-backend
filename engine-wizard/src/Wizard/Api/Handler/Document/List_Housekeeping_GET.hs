module Wizard.Api.Handler.Document.List_Housekeeping_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Document.DocumentService

type List_Housekeeping_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "documents"
     :> "housekeeping"
     :> Verb GET 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

list_housekeeping_GET ::
     Maybe String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
list_housekeeping_GET mServiceToken mServerUrl =
  runInUnauthService mServerUrl $
  addTraceUuidHeader =<< do
    checkServiceToken mServiceToken
    cleanDocuments
    return NoContent

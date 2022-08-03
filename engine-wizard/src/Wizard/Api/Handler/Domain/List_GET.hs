module Wizard.Api.Handler.Domain.List_GET where

import Data.Maybe (fromMaybe)
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.App.AppValidation

type List_GET
   = Header "Host" String
     :> "domains"
     :> QueryParam "check-domain" String
     :> Verb GET 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

list_GET :: Maybe String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
list_GET mServerUrl mAppId =
  runInUnauthService mServerUrl NoTransaction $
  addTraceUuidHeader =<< do
    validateAppId (fromMaybe "" mAppId)
    return NoContent

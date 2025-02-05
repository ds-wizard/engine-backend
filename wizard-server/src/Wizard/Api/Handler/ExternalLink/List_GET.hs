module Wizard.Api.Handler.ExternalLink.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Model.Error.Error
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import WizardLib.Public.Service.ExternalLink.ExternalLinkUsageService

type List_GET =
  Header "Host" String
    :> "external-link"
    :> QueryParam' '[Required] "url" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_GET :: Maybe String -> String -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_GET mServerUrl url =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< do
      createExternalLinkUsage url
      throwError $ FoundError url

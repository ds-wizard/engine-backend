module Wizard.Api.Handler.Locale.List_Current_Content_GET where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.Locale.LocaleService

type List_Current_Content_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> "current"
    :> "content"
    :> QueryParam "clientUrl" String
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)

list_current_content_GET :: Maybe String -> Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)
list_current_content_GET mTokenHeader mServerUrl mClientUrl =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ do
      locale <- getLocaleContentForCurrentUser mClientUrl
      traceUuid <- asks traceUuid
      return . addHeader (U.toString traceUuid) . addHeader "application/json" . FileStream $ locale

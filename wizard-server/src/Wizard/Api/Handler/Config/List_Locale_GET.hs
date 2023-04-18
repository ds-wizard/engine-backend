module Wizard.Api.Handler.Config.List_Locale_GET where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.Locale.LocaleService

type List_Locale_GET =
  Header "Host" String
    :> "configs"
    :> "locales"
    :> Capture "localeId" String
    :> QueryParam "clientUrl" String
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)

list_locale_GET
  :: Maybe String
  -> String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)
list_locale_GET mServerUrl localeId mClientUrl =
  runInUnauthService mServerUrl NoTransaction $ do
    locale <- getLocaleContentForId localeId mClientUrl
    traceUuid <- asks traceUuid
    return . addHeader (U.toString traceUuid) . addHeader "application/json" . FileStream $ locale

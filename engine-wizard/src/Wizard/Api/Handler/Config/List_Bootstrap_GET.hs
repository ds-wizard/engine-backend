module Wizard.Api.Handler.Config.List_Bootstrap_GET where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS_Char
import Data.Maybe (fromMaybe)
import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.ClientConfigJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.ClientConfigService

type List_Bootstrap_GET
   = "configs"
     :> "bootstrap"
     :> QueryParam "callback" String
     :> Get '[ ApplicationJavascript] (Headers '[ Header "x-trace-uuid" String] Javascript)

list_bootstrap_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] Javascript)
list_bootstrap_GET mCallbackName =
  runInUnauthService $
  addTraceUuidHeader =<< do
    let callbackName = fromMaybe "callback" mCallbackName
    resDto <- getClientConfig
    let content = LBS.append (LBS_Char.pack callbackName) (LBS.append "(" (LBS.append (encode resDto) ")"))
    return . Javascript $ content
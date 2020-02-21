module Wizard.Api.Handler.Config.List_GET where

import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.ClientConfigJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.ClientConfigService

type List_GET
   = "configuration"
     :> QueryParam "callback" String
     :> Get '[ ApplicationJavascript] (Headers '[ Header "x-trace-uuid" String] String)

list_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] String)
list_GET mCallbackName =
  runInUnauthService $
  addTraceUuidHeader =<< do
    let callbackName = fromMaybe "callback" mCallbackName
    resDto <- getClientConfig
    let content = callbackName ++ "(" ++ (LT.unpack . decodeUtf8 . encode $ resDto) ++ ")"
    return content

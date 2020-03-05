module Wizard.Api.Handler.IO.Api where

import Servant

import Wizard.Api.Handler.IO.Export_GET
import Wizard.Api.Handler.IO.Import_POST
import Wizard.Model.Context.BaseContext

type IoAPI
   = Import_POST
     :<|> Export_GET

ioApi :: Proxy IoAPI
ioApi = Proxy

ioServer :: ServerT IoAPI BaseContextM
ioServer = import_POST :<|> export_GET

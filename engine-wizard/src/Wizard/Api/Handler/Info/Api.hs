module Wizard.Api.Handler.Info.Api where

import Servant

import Wizard.Api.Handler.Info.List_GET
import Wizard.Model.Context.BaseContext

type InfoAPI = List_GET

infoApi :: Proxy InfoAPI
infoApi = Proxy

infoServer :: ServerT InfoAPI BaseContextM
infoServer = list_GET

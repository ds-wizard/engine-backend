module Wizard.Api.Handler.Dev.Api where

import Servant

import Wizard.Api.Handler.Dev.Operation.Api
import Wizard.Model.Context.BaseContext

type DevAPI = DevOperationAPI

devApi :: Proxy DevAPI
devApi = Proxy

devServer :: ServerT DevAPI BaseContextM
devServer = devOperationServer

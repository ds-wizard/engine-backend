module Wizard.Api.Handler.Level.Api where

import Servant

import Wizard.Api.Handler.Level.List_GET
import Wizard.Model.Context.BaseContext

type LevelAPI = List_GET

levelApi :: Proxy LevelAPI
levelApi = Proxy

levelServer :: ServerT LevelAPI BaseContextM
levelServer = list_GET

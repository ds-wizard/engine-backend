module Wizard.Api.Handler.Template.Api where

import Servant

import Wizard.Api.Handler.Template.List_GET
import Wizard.Model.Context.BaseContext

type TemplateAPI = List_GET

templateApi :: Proxy TemplateAPI
templateApi = Proxy

templateServer :: ServerT TemplateAPI BaseContextM
templateServer = list_GET

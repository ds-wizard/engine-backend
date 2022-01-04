module Wizard.Api.Handler.KnowledgeModel.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.KnowledgeModel.List_POST
import Wizard.Model.Context.BaseContext

type KnowledgeModelAPI
   = Tags "Knowledge Model"
     :> List_POST

knowledgeModelApi :: Proxy KnowledgeModelAPI
knowledgeModelApi = Proxy

knowledgeModelServer :: ServerT KnowledgeModelAPI BaseContextM
knowledgeModelServer = list_POST

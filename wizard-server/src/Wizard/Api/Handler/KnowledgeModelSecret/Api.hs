module Wizard.Api.Handler.KnowledgeModelSecret.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.KnowledgeModelSecret.Detail_DELETE
import Wizard.Api.Handler.KnowledgeModelSecret.Detail_PUT
import Wizard.Api.Handler.KnowledgeModelSecret.List_GET
import Wizard.Api.Handler.KnowledgeModelSecret.List_POST
import Wizard.Model.Context.BaseContext

type KnowledgeModelSecretAPI =
  Tags "KnowledgeModelSecret"
    :> ( List_GET
          :<|> List_POST
          :<|> Detail_PUT
          :<|> Detail_DELETE
       )

knowledgeModelSecretApi :: Proxy KnowledgeModelSecretAPI
knowledgeModelSecretApi = Proxy

knowledgeModelSecretServer :: ServerT KnowledgeModelSecretAPI BaseContextM
knowledgeModelSecretServer =
  list_GET
    :<|> list_POST
    :<|> detail_PUT
    :<|> detail_DELETE

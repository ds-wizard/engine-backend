module Wizard.Api.Handler.KnowledgeModelEditor.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.KnowledgeModelEditor.Detail_DELETE
import Wizard.Api.Handler.KnowledgeModelEditor.Detail_GET
import Wizard.Api.Handler.KnowledgeModelEditor.Detail_PUT
import Wizard.Api.Handler.KnowledgeModelEditor.Detail_WS
import Wizard.Api.Handler.KnowledgeModelEditor.List_GET
import Wizard.Api.Handler.KnowledgeModelEditor.List_POST
import Wizard.Api.Handler.KnowledgeModelEditor.List_Suggestions_GET
import Wizard.Model.Context.BaseContext

type KnowledgeModelEditorAPI =
  Tags "Knowledge Model Editor"
    :> ( List_GET
          :<|> List_Suggestions_GET
          :<|> List_POST
          :<|> Detail_GET
          :<|> Detail_PUT
          :<|> Detail_DELETE
          :<|> Detail_WS
       )

knowledgeModelEditorApi :: Proxy KnowledgeModelEditorAPI
knowledgeModelEditorApi = Proxy

knowledgeModelEditorServer :: ServerT KnowledgeModelEditorAPI BaseContextM
knowledgeModelEditorServer =
  list_GET
    :<|> list_suggestions_GET
    :<|> list_POST
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_DELETE
    :<|> detail_WS

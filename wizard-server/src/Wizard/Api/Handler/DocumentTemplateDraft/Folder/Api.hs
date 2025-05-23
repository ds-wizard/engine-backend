module Wizard.Api.Handler.DocumentTemplateDraft.Folder.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.DocumentTemplateDraft.Folder.List_Delete_POST
import Wizard.Api.Handler.DocumentTemplateDraft.Folder.List_Move_POST
import Wizard.Model.Context.BaseContext

type DocumentTemplateFolderAPI =
  Tags "Document Template Draft Folder"
    :> ( List_Delete_POST
          :<|> List_Move_POST
       )

documentTemplateFolderApi :: Proxy DocumentTemplateFolderAPI
documentTemplateFolderApi = Proxy

documentTemplateFolderServer :: ServerT DocumentTemplateFolderAPI BaseContextM
documentTemplateFolderServer =
  list_delete_POST
    :<|> list_move_POST

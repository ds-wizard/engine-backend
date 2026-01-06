module Wizard.Api.Handler.Project.File.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Project.File.Detail_DELETE
import Wizard.Api.Handler.Project.File.Detail_Download_GET
import Wizard.Api.Handler.Project.File.List_GET
import Wizard.Api.Handler.Project.File.List_POST
import Wizard.Model.Context.BaseContext

type FileAPI =
  Tags "Project File"
    :> ( List_GET
          :<|> List_POST
          :<|> Detail_DELETE
          :<|> Detail_Download_GET
       )

fileApi :: Proxy FileAPI
fileApi = Proxy

fileServer :: ServerT FileAPI BaseContextM
fileServer =
  list_GET
    :<|> list_POST
    :<|> detail_DELETE
    :<|> detail_download_GET

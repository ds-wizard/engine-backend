module Wizard.Api.Handler.Project.Version.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Project.Version.Detail_DELETE
import Wizard.Api.Handler.Project.Version.Detail_PUT
import Wizard.Api.Handler.Project.Version.List_GET
import Wizard.Api.Handler.Project.Version.List_POST
import Wizard.Model.Context.BaseContext

type VersionAPI =
  Tags "Project Version"
    :> ( List_GET
          :<|> List_POST
          :<|> Detail_PUT
          :<|> Detail_DELETE
       )

versionApi :: Proxy VersionAPI
versionApi = Proxy

versionServer :: ServerT VersionAPI BaseContextM
versionServer =
  list_GET
    :<|> list_POST
    :<|> detail_PUT
    :<|> detail_DELETE

module Wizard.Api.Handler.ProjectImporter.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.ProjectImporter.Detail_GET
import Wizard.Api.Handler.ProjectImporter.Detail_PUT
import Wizard.Api.Handler.ProjectImporter.List_GET
import Wizard.Api.Handler.ProjectImporter.List_Suggestions_GET
import Wizard.Model.Context.BaseContext

type ProjectImporterAPI =
  Tags "Project Importer"
    :> ( List_GET
          :<|> List_Suggestions_GET
          :<|> Detail_GET
          :<|> Detail_PUT
       )

projectImporterApi :: Proxy ProjectImporterAPI
projectImporterApi = Proxy

projectImporterServer :: ServerT ProjectImporterAPI BaseContextM
projectImporterServer =
  list_GET
    :<|> list_suggestions_GET
    :<|> detail_GET
    :<|> detail_PUT

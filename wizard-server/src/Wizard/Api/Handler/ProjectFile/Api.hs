module Wizard.Api.Handler.ProjectFile.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.ProjectFile.List_GET
import Wizard.Model.Context.BaseContext

type ProjectFileAPI =
  Tags "Project File"
    :> List_GET

projectFileApi :: Proxy ProjectFileAPI
projectFileApi = Proxy

projectFileServer :: ServerT ProjectFileAPI BaseContextM
projectFileServer =
  list_GET

module Wizard.Api.Handler.Project.Migration.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Project.Migration.List_POST
import Wizard.Model.Context.BaseContext

type MigrationAPI =
  Tags "Project Migration"
    :> List_POST

migrationApi :: Proxy MigrationAPI
migrationApi = Proxy

migrationServer :: ServerT MigrationAPI BaseContextM
migrationServer = list_POST

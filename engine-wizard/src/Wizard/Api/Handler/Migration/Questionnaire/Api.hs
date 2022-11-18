module Wizard.Api.Handler.Migration.Questionnaire.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Migration.Questionnaire.List_Current_Completion_POST
import Wizard.Api.Handler.Migration.Questionnaire.List_Current_DELETE
import Wizard.Api.Handler.Migration.Questionnaire.List_Current_GET
import Wizard.Api.Handler.Migration.Questionnaire.List_Current_PUT
import Wizard.Api.Handler.Migration.Questionnaire.List_POST
import Wizard.Model.Context.BaseContext

type MigrationAPI =
  Tags "Questionnaire Migration"
    :> ( List_POST
          :<|> List_Current_GET
          :<|> List_Current_PUT
          :<|> List_Current_DELETE
          :<|> List_Current_Completion_POST
       )

migrationApi :: Proxy MigrationAPI
migrationApi = Proxy

migrationServer :: ServerT MigrationAPI BaseContextM
migrationServer =
  list_POST :<|> list_current_GET :<|> list_current_PUT :<|> list_current_DELETE :<|> list_current_completion_POST

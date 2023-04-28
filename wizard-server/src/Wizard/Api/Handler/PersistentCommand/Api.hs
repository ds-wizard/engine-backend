module Wizard.Api.Handler.PersistentCommand.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.PersistentCommand.Detail_Attempts_POST
import Wizard.Api.Handler.PersistentCommand.Detail_GET
import Wizard.Api.Handler.PersistentCommand.Detail_PUT
import Wizard.Api.Handler.PersistentCommand.List_Attempts_POST
import Wizard.Api.Handler.PersistentCommand.List_GET
import Wizard.Api.Handler.PersistentCommand.List_POST
import Wizard.Model.Context.BaseContext

type PersistentCommandAPI =
  Tags "Persistent Command"
    :> ( List_GET
          :<|> List_POST
          :<|> List_Attempts_POST
          :<|> Detail_GET
          :<|> Detail_PUT
          :<|> Detail_Attempts_POST
       )

persistentCommandApi :: Proxy PersistentCommandAPI
persistentCommandApi = Proxy

persistentCommandServer :: ServerT PersistentCommandAPI BaseContextM
persistentCommandServer = list_GET :<|> list_POST :<|> list_attempts_POST :<|> detail_GET :<|> detail_PUT :<|> detail_attempts_POST

module Registry.Api.Handler.PersistentCommand.Api where

import Servant

import Registry.Api.Handler.PersistentCommand.List_POST
import Registry.Model.Context.BaseContext
import RegistryLib.Api.Handler.PersistentCommand.List_POST

type PersistentCommandAPI =
  List_POST

persistentCommandApi :: Proxy PersistentCommandAPI
persistentCommandApi = Proxy

persistentCommandServer :: ServerT PersistentCommandAPI BaseContextM
persistentCommandServer = list_POST

module RegistryLib.Api.Handler.PersistentCommand.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand

type List_POST =
  Header "Authorization" String
    :> ReqBody '[SafeJSON] (PersistentCommand String)
    :> "persistent-commands"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (PersistentCommand String))

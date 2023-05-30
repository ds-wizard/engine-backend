module Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandSM where

import Data.Swagger

import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand

instance ToSchema PersistentCommandState

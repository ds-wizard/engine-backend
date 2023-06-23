module Wizard.Database.Migration.Development.PersistentCommand.Data.PersistentCommands where

import qualified Data.UUID as U

import Shared.Common.Constant.App
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand

command1 :: PersistentCommand U.UUID
command1 =
  PersistentCommand
    { uuid = u' "34493424-ad08-4752-acf4-ac92223dc2f6"
    , state = DonePersistentCommandState
    , component = "component1"
    , function = "function1"
    , body = "{}"
    , lastErrorMessage = Nothing
    , attempts = 1
    , maxAttempts = 10
    , internal = True
    , destination = Nothing
    , appUuid = defaultAppUuid
    , createdBy = Nothing
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }

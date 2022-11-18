module Wizard.Database.Migration.Development.PersistentCommand.Data.PersistentCommands where

import Shared.Constant.App
import Shared.Util.Date
import Shared.Util.Uuid
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.PersistentCommand.PersistentCommand
import Wizard.Model.User.User

command1 :: PersistentCommand
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
    , appUuid = defaultAppUuid
    , createdBy = Just $ userAlbert.uuid
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }

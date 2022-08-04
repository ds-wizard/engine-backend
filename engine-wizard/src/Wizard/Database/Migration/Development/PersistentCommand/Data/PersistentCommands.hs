module Wizard.Database.Migration.Development.PersistentCommand.Data.PersistentCommands where

import Control.Lens ((^.))

import LensesConfig
import Shared.Constant.App
import Shared.Util.Date
import Shared.Util.Uuid
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.PersistentCommand.PersistentCommand

command1 :: PersistentCommand
command1 =
  PersistentCommand
    { _persistentCommandUuid = u' "34493424-ad08-4752-acf4-ac92223dc2f6"
    , _persistentCommandState = DonePersistentCommandState
    , _persistentCommandComponent = "component1"
    , _persistentCommandFunction = "function1"
    , _persistentCommandBody = "{}"
    , _persistentCommandLastErrorMessage = Nothing
    , _persistentCommandAttempts = 1
    , _persistentCommandMaxAttempts = 10
    , _persistentCommandInternal = True
    , _persistentCommandAppUuid = defaultAppUuid
    , _persistentCommandCreatedBy = Just $ userAlbert ^. uuid
    , _persistentCommandCreatedAt = dt' 2018 1 25
    , _persistentCommandUpdatedAt = dt' 2018 1 25
    }

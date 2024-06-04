module WizardLib.Public.Database.Migration.Development.PersistentCommand.Data.PersistentCommands where

import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import WizardLib.Public.Database.Migration.Development.Tenant.Data.Tenants
import WizardLib.Public.Database.Migration.Development.User.Data.Users
import WizardLib.Public.Model.PersistentCommand.PersistentCommandList

command1List :: PersistentCommandList
command1List =
  PersistentCommandList
    { uuid = u' "34493424-ad08-4752-acf4-ac92223dc2f6"
    , state = DonePersistentCommandState
    , component = "component1"
    , function = "function1"
    , attempts = 1
    , maxAttempts = 10
    , tenant = tenantSuggestion
    , createdBy = Just userAlbertSuggestion
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }

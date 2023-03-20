module Wizard.Api.Resource.PersistentCommand.PersistentCommandChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.App.AppSM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandChangeDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandChangeJM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Wizard.Api.Resource.User.UserSuggestionSM ()
import Wizard.Model.PersistentCommand.PersistentCommand

instance ToSchema PersistentCommandChangeDTO where
  declareNamedSchema = toSwagger (PersistentCommandChangeDTO {state = IgnorePersistentCommandState})

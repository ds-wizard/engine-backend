module WizardLib.Public.Api.Resource.PersistentCommand.PersistentCommandListJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandJM ()
import WizardLib.Public.Api.Resource.Tenant.TenantSuggestionJM ()
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()
import WizardLib.Public.Model.PersistentCommand.PersistentCommandList

instance FromJSON PersistentCommandList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PersistentCommandList where
  toJSON = genericToJSON jsonOptions

module Wizard.Api.Resource.User.Group.UserGroupSuggestionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.User.Group.UserGroupSuggestionJM ()
import Wizard.Model.User.UserGroupSuggestion
import Wizard.Service.User.Group.UserGroupMapper
import WizardLib.Public.Database.Migration.Development.User.Data.UserGroups

instance ToSchema UserGroupSuggestion where
  declareNamedSchema = toSwagger (toSuggestion bioGroup)

module Wizard.Database.Mapping.User.UserGroupSuggestion where

import Database.PostgreSQL.Simple

import Wizard.Model.User.UserGroupSuggestion

instance FromRow UserGroupSuggestion

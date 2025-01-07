module Wizard.Database.Mapping.Branch.BranchSuggestion where

import Database.PostgreSQL.Simple

import Wizard.Model.Branch.BranchSuggestion

instance FromRow BranchSuggestion

module Wizard.Database.Mapping.Plan.AppPlan where

import Database.PostgreSQL.Simple

import Wizard.Model.Plan.AppPlan

instance ToRow AppPlan

instance FromRow AppPlan

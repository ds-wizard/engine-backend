module Wizard.Database.Mapping.Limit.AppLimit where

import Database.PostgreSQL.Simple

import Wizard.Model.Limit.AppLimit

instance ToRow AppLimit

instance FromRow AppLimit

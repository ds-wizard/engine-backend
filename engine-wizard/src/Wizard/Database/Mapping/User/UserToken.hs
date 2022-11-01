module Wizard.Database.Mapping.User.UserToken where

import Database.PostgreSQL.Simple

import Wizard.Model.User.UserToken

instance ToRow UserToken

instance FromRow UserToken

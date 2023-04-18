module Wizard.Database.Mapping.Acl.Acl where

import Database.PostgreSQL.Simple

import Wizard.Model.Acl.Acl

instance ToRow Group

instance FromRow Group

module Wizard.Database.Mapping.Audit.Audit where

import Database.PostgreSQL.Simple

import Shared.Common.Database.Mapping.Common ()
import Wizard.Model.Audit.Audit

instance FromRow Audit

instance ToRow Audit

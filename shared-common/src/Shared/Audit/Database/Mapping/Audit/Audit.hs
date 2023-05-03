module Shared.Audit.Database.Mapping.Audit.Audit where

import Database.PostgreSQL.Simple

import Shared.Audit.Model.Audit.Audit
import Shared.Common.Database.Mapping.Common ()

instance FromRow Audit

instance ToRow Audit

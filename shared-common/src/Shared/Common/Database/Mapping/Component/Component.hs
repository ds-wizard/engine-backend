module Shared.Common.Database.Mapping.Component.Component where

import Database.PostgreSQL.Simple

import Shared.Common.Model.Component.Component

instance ToRow Component

instance FromRow Component

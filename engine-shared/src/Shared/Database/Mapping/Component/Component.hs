module Shared.Database.Mapping.Component.Component where

import Database.PostgreSQL.Simple

import Shared.Model.Component.Component

instance ToRow Component

instance FromRow Component

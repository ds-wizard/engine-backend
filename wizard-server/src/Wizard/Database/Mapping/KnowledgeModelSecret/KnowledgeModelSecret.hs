module Wizard.Database.Mapping.KnowledgeModelSecret.KnowledgeModelSecret where

import Database.PostgreSQL.Simple

import Wizard.Model.KnowledgeModelSecret.KnowledgeModelSecret

instance ToRow KnowledgeModelSecret

instance FromRow KnowledgeModelSecret

module Wizard.Database.Mapping.KnowledgeModel.Secret.KnowledgeModelSecret where

import Database.PostgreSQL.Simple

import Wizard.Model.KnowledgeModel.KnowledgeModelSecret

instance ToRow KnowledgeModelSecret

instance FromRow KnowledgeModelSecret

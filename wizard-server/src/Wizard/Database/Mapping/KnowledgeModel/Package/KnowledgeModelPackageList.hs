module Wizard.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageList where

import Database.PostgreSQL.Simple

import Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackagePhase ()
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageList

instance FromRow KnowledgeModelPackageList

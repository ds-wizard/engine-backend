module Wizard.Database.Mapping.Package.PackageList where

import Database.PostgreSQL.Simple

import Wizard.Model.Package.PackageList
import WizardLib.KnowledgeModel.Database.Mapping.Package.PackagePhase ()

instance FromRow PackageList

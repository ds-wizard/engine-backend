module Wizard.Api.Resource.Package.PackageChangeDTO where

import GHC.Generics

import WizardLib.KnowledgeModel.Model.Package.Package

data PackageChangeDTO = PackageChangeDTO
  { phase :: PackagePhase
  }
  deriving (Show, Eq, Generic)

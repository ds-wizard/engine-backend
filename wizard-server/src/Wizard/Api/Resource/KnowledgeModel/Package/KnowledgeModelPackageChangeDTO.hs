module Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO where

import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

data KnowledgeModelPackageChangeDTO = KnowledgeModelPackageChangeDTO
  { phase :: KnowledgeModelPackagePhase
  }
  deriving (Show, Eq, Generic)

module WizardLib.KnowledgeModel.Model.Package.PackageGroup where

import GHC.Generics

data PackageGroup = PackageGroup
  { organizationId :: String
  , kmId :: String
  , versions :: String
  }
  deriving (Show, Eq, Generic)

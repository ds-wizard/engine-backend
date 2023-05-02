module WizardLib.KnowledgeModel.Model.PackageBundle.PackageBundle where

import GHC.Generics

import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

data PackageBundle = PackageBundle
  { bundleId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , metamodelVersion :: Int
  , packages :: [PackageWithEvents]
  }
  deriving (Show, Eq, Generic)

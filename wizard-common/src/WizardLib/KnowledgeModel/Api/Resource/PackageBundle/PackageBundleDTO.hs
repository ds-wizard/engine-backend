module WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleDTO where

import GHC.Generics
import WizardLib.KnowledgeModel.Api.Resource.Package.PackageDTO

data PackageBundleDTO = PackageBundleDTO
  { bundleId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , metamodelVersion :: Int
  , packages :: [PackageDTO]
  }
  deriving (Show, Eq, Generic)

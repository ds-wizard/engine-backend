module WizardLib.KnowledgeModel.Service.Package.Bundle.PackageBundleMapper where

import WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleDTO
import WizardLib.KnowledgeModel.Model.PackageBundle.PackageBundle
import qualified WizardLib.KnowledgeModel.Service.Package.PackageMapper as PM

toDTO :: PackageBundle -> PackageBundleDTO
toDTO pb =
  PackageBundleDTO
    { bundleId = pb.bundleId
    , name = pb.name
    , organizationId = pb.organizationId
    , kmId = pb.kmId
    , version = pb.version
    , metamodelVersion = pb.metamodelVersion
    , packages = PM.toDTO <$> pb.packages
    }

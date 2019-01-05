module Service.KnowledgeModelBundle.KnowledgeModelBundleMapper where

import Control.Lens ((^.))

import Api.Resource.KnowledgeModelBundle.KnowledgeModelBundleDTO
import LensesConfig
import Model.KnowledgeModelBundle.KnowledgeModelBundle
import Service.Package.PackageMapper

toDTO :: KnowledgeModelBundle -> KnowledgeModelBundleDTO
toDTO kmb =
  KnowledgeModelBundleDTO
  { _knowledgeModelBundleDTOBundleId = kmb ^. bundleId
  , _knowledgeModelBundleDTOName = kmb ^. name
  , _knowledgeModelBundleDTOOrganizationId = kmb ^. organizationId
  , _knowledgeModelBundleDTOKmId = kmb ^. kmId
  , _knowledgeModelBundleDTOVersion = kmb ^. version
  , _knowledgeModelBundleDTOPackages = packageWithEventsToDTOWithEvents <$> kmb ^. packages
  }

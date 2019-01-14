module Database.Migration.Development.KnowledgeModelBundle.Data.KnowledgeModelBundles where

import Control.Lens ((^.))

import Database.Migration.Development.Package.Data.Packages
import LensesConfig
import Model.KnowledgeModelBundle.KnowledgeModelBundle

elixirNlPackage2DtoKMBudle =
  KnowledgeModelBundle
  { _knowledgeModelBundleBundleId = elixirNlPackage2Dto ^. pId
  , _knowledgeModelBundleName = elixirNlPackage2Dto ^. name
  , _knowledgeModelBundleOrganizationId = elixirNlPackage2Dto ^. organizationId
  , _knowledgeModelBundleKmId = elixirNlPackage2Dto ^. kmId
  , _knowledgeModelBundleVersion = elixirNlPackage2Dto ^. version
  , _knowledgeModelBundlePackages = [baseElixirPackageDto, elixirNlPackageDto, elixirNlPackage2Dto]
  }

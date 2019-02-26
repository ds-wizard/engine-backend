module Api.Resource.KnowledgeModelBundle.KnowledgeModelBundleDTO where

import Api.Resource.Package.PackageWithEventsDTO

data KnowledgeModelBundleDTO = KnowledgeModelBundleDTO
  { _knowledgeModelBundleDTOBundleId :: String
  , _knowledgeModelBundleDTOName :: String
  , _knowledgeModelBundleDTOOrganizationId :: String
  , _knowledgeModelBundleDTOKmId :: String
  , _knowledgeModelBundleDTOVersion :: String
  , _knowledgeModelBundleDTOMetamodelVersion :: Int
  , _knowledgeModelBundleDTOPackages :: [PackageWithEventsDTO]
  } deriving (Show, Eq)

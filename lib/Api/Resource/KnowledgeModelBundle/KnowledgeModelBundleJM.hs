module Api.Resource.KnowledgeModelBundle.KnowledgeModelBundleJM where

import Control.Monad
import Data.Aeson

import Api.Resource.KnowledgeModelBundle.KnowledgeModelBundleDTO
import Api.Resource.Package.PackageWithEventsDTO ()

instance FromJSON KnowledgeModelBundleDTO where
  parseJSON (Object o) = do
    _knowledgeModelBundleDTOBundleId <- o .: "id"
    _knowledgeModelBundleDTOName <- o .: "name"
    _knowledgeModelBundleDTOOrganizationId <- o .: "organizationId"
    _knowledgeModelBundleDTOKmId <- o .: "kmId"
    _knowledgeModelBundleDTOVersion <- o .: "version"
    packagesSerialized <- o .: "packages"
    _knowledgeModelBundleDTOPackages <- parseJSON packagesSerialized
    return KnowledgeModelBundleDTO {..}
  parseJSON _ = mzero

instance ToJSON KnowledgeModelBundleDTO where
  toJSON KnowledgeModelBundleDTO {..} =
    object
      [ "id" .= _knowledgeModelBundleDTOBundleId
      , "name" .= _knowledgeModelBundleDTOName
      , "organizationId" .= _knowledgeModelBundleDTOOrganizationId
      , "kmId" .= _knowledgeModelBundleDTOKmId
      , "version" .= _knowledgeModelBundleDTOVersion
      , "packages" .= toJSON _knowledgeModelBundleDTOPackages
      ]

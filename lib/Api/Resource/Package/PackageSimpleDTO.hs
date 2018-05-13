module Api.Resource.Package.PackageSimpleDTO where

import Data.Aeson

data PackageSimpleDTO = PackageSimpleDTO
  { _packageSimpleDTOName :: String
  , _packageSimpleDTOOrganizationId :: String
  , _packageSimpleDTOArtifactId :: String
  } deriving (Show, Eq)

instance ToJSON PackageSimpleDTO where
  toJSON PackageSimpleDTO {..} =
    object
      [ "name" .= _packageSimpleDTOName
      , "organizationId" .= _packageSimpleDTOOrganizationId
      , "artifactId" .= _packageSimpleDTOArtifactId
      ]

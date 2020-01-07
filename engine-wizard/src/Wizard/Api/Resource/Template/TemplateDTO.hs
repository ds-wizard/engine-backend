module Wizard.Api.Resource.Template.TemplateDTO where

import qualified Data.UUID as U
import GHC.Generics

data TemplateDTO =
  TemplateDTO
    { _templateDTOUuid :: U.UUID
    , _templateDTOName :: String
    , _templateDTORootFile :: String
    , _templateDTOAllowedKMs :: [TemplateAllowedKMDTO]
    }
  deriving (Show, Generic)

data TemplateAllowedKMDTO =
  TemplateAllowedKMDTO
    { _templateAllowedKMDTOOrgId :: Maybe String
    , _templateAllowedKMDTOKmId :: Maybe String
    , _templateAllowedKMDTOMinVersion :: Maybe String
    , _templateAllowedKMDTOMaxVersion :: Maybe String
    }
  deriving (Show, Generic)

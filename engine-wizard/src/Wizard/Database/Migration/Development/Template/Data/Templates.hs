module Wizard.Database.Migration.Development.Template.Data.Templates where

import Data.Maybe (fromJust)
import qualified Data.UUID as U

import Wizard.Api.Resource.Template.TemplateDTO

commonWizardTemplate =
  TemplateDTO
    { _templateDTOUuid = fromJust $ U.fromString "43a3fdd1-8535-42e0-81a7-5edbff296e65"
    , _templateDTOName = "Default Template"
    , _templateDTOAllowedKMs =
        [ TemplateAllowedKMDTO
            { _templateAllowedKMDTOOrgId = Nothing
            , _templateAllowedKMDTOKmId = Nothing
            , _templateAllowedKMDTOMinVersion = Nothing
            , _templateAllowedKMDTOMaxVersion = Nothing
            }
        ]
    , _templateDTOFormats =
        [ TemplateFormatDTO
            { _templateFormatDTOUuid = fromJust $ U.fromString "329521df-8fa1-4d7a-b9c5-49f75f5d31d5"
            , _templateFormatDTOName = "PDF"
            , _templateFormatDTOIcon = "far fa-file-pdf"
            }
        , TemplateFormatDTO
            { _templateFormatDTOUuid = fromJust $ U.fromString "fabab18b-2497-4878-8318-1dccdb016ff2"
            , _templateFormatDTOName = "RDF"
            , _templateFormatDTOIcon = "far fa-file"
            }
        ]
    }

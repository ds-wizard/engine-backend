module Wizard.Database.Migration.Development.Template.Data.Templates where

import Data.Maybe (fromJust)
import qualified Data.UUID as U

import Wizard.Api.Resource.Template.TemplateDTO

commonWizardTemplate =
  TemplateDTO
    { _templateDTOUuid = fromJust $ U.fromString "43a3fdd1-8535-42e0-81a7-5edbff296e65"
    , _templateDTOName = "Default Template"
    , _templateDTORootFile = "root.html.j2"
    , _templateDTOAllowedKMs =
        [ TemplateAllowedKMDTO
            { _templateAllowedKMDTOOrgId = Nothing
            , _templateAllowedKMDTOKmId = Nothing
            , _templateAllowedKMDTOMinVersion = Nothing
            , _templateAllowedKMDTOMaxVersion = Nothing
            }
        ]
    }

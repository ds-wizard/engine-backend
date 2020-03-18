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
            { _templateFormatDTOUuid = fromJust $ U.fromString "d3e98eb6-344d-481f-8e37-6a67b6cd1ad2"
            , _templateFormatDTOName = "JSON Data"
            , _templateFormatDTOIcon = "far fa-file"
            }
        , TemplateFormatDTO
            { _templateFormatDTOUuid = fromJust $ U.fromString "a9293d08-59a4-4e6b-ae62-7a6a570b031c"
            , _templateFormatDTOName = "HTML Document"
            , _templateFormatDTOIcon = "far fa-file-code"
            }
        , TemplateFormatDTO
            { _templateFormatDTOUuid = fromJust $ U.fromString "68c26e34-5e77-4e15-9bf7-06ff92582257"
            , _templateFormatDTOName = "PDF Document"
            , _templateFormatDTOIcon = "far fa-file-pdf"
            }
        , TemplateFormatDTO
            { _templateFormatDTOUuid = fromJust $ U.fromString "dbc94579-40d7-42c3-975c-71e30d07778b"
            , _templateFormatDTOName = "LaTeX Document"
            , _templateFormatDTOIcon = "far fa-file-alt"
            }
        , TemplateFormatDTO
            { _templateFormatDTOUuid = fromJust $ U.fromString "f4bd941a-dfbe-4226-a1fc-200fb5269311"
            , _templateFormatDTOName = "MS Word Document"
            , _templateFormatDTOIcon = "far fa-file-word"
            }
        , TemplateFormatDTO
            { _templateFormatDTOUuid = fromJust $ U.fromString "15e53172-bbae-4a0c-a4d9-8f3ddf60e7b6"
            , _templateFormatDTOName = "OpenDocument Text"
            , _templateFormatDTOIcon = "far fa-file-alt"
            }
        , TemplateFormatDTO
            { _templateFormatDTOUuid = fromJust $ U.fromString "f0533e48-f4c5-4af2-b2c1-5a47d4a247c0"
            , _templateFormatDTOName = "Markdown Document"
            , _templateFormatDTOIcon = "far fa-file-alt"
            }
        ]
    }

module Wizard.Database.Migration.Development.Template.Data.Templates where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Model.Template.Template

commonWizardTemplate =
  Template
    { _templateUuid = fromJust $ U.fromString "43a3fdd1-8535-42e0-81a7-5edbff296e65"
    , _templateName = "Default Template"
    , _templateDescription = "This is a default template"
    , _templateAllowedPackages = [templateAllowedPackage]
    , _templateRecommendedPackageId = Nothing
    , _templateFormats =
        [ templateFormatJson
        , templateFormatHtml
        , templateFormatPdf
        , templateFormatLatex
        , templateFormatDocx
        , templateFormatOdt
        , templateFormatMarkdown
        ]
    }

templateAllowedPackage :: TemplateAllowedPackage
templateAllowedPackage =
  TemplateAllowedPackage
    { _templateAllowedPackageOrgId = Nothing
    , _templateAllowedPackageKmId = Nothing
    , _templateAllowedPackageMinVersion = Nothing
    , _templateAllowedPackageMaxVersion = Nothing
    }

templateFormatJson :: TemplateFormat
templateFormatJson =
  TemplateFormat
    { _templateFormatUuid = fromJust $ U.fromString "d3e98eb6-344d-481f-8e37-6a67b6cd1ad2"
    , _templateFormatName = "JSON Data"
    , _templateFormatShortName = "json"
    , _templateFormatIcon = "far fa-file"
    , _templateFormatColor = "#f15a24"
    }

templateFormatHtml :: TemplateFormat
templateFormatHtml =
  TemplateFormat
    { _templateFormatUuid = fromJust $ U.fromString "a9293d08-59a4-4e6b-ae62-7a6a570b031c"
    , _templateFormatName = "HTML Document"
    , _templateFormatShortName = "html"
    , _templateFormatIcon = "far fa-file-code"
    , _templateFormatColor = "#f15a24"
    }

templateFormatPdf :: TemplateFormat
templateFormatPdf =
  TemplateFormat
    { _templateFormatUuid = fromJust $ U.fromString "68c26e34-5e77-4e15-9bf7-06ff92582257"
    , _templateFormatName = "PDF Document"
    , _templateFormatShortName = "pdf"
    , _templateFormatIcon = "far fa-file-pdf"
    , _templateFormatColor = "#f15a24"
    }

templateFormatLatex :: TemplateFormat
templateFormatLatex =
  TemplateFormat
    { _templateFormatUuid = fromJust $ U.fromString "dbc94579-40d7-42c3-975c-71e30d07778b"
    , _templateFormatName = "LaTeX Document"
    , _templateFormatShortName = "latex"
    , _templateFormatIcon = "far fa-file-alt"
    , _templateFormatColor = "#f15a24"
    }

templateFormatDocx :: TemplateFormat
templateFormatDocx =
  TemplateFormat
    { _templateFormatUuid = fromJust $ U.fromString "f4bd941a-dfbe-4226-a1fc-200fb5269311"
    , _templateFormatName = "MS Word Document"
    , _templateFormatShortName = "docx"
    , _templateFormatIcon = "far fa-file-word"
    , _templateFormatColor = "#f15a24"
    }

templateFormatOdt :: TemplateFormat
templateFormatOdt =
  TemplateFormat
    { _templateFormatUuid = fromJust $ U.fromString "15e53172-bbae-4a0c-a4d9-8f3ddf60e7b6"
    , _templateFormatName = "OpenDocument Text"
    , _templateFormatShortName = "odt"
    , _templateFormatIcon = "far fa-file-alt"
    , _templateFormatColor = "#f15a24"
    }

templateFormatMarkdown :: TemplateFormat
templateFormatMarkdown =
  TemplateFormat
    { _templateFormatUuid = fromJust $ U.fromString "f0533e48-f4c5-4af2-b2c1-5a47d4a247c0"
    , _templateFormatName = "Markdown Document"
    , _templateFormatShortName = "md"
    , _templateFormatIcon = "far fa-file-alt"
    , _templateFormatColor = "#f15a24"
    }

commonWizardTemplateDTO :: TemplateDTO
commonWizardTemplateDTO =
  TemplateDTO
    { _templateDTOUuid = commonWizardTemplate ^. uuid
    , _templateDTOName = commonWizardTemplate ^. name
    , _templateDTODescription = commonWizardTemplate ^. description
    , _templateDTOAllowedPackages = []
    , _templateDTORecommendedPackageId = Nothing
    , _templateDTOFormats =
        [ templateFormatJson
        , templateFormatHtml
        , templateFormatPdf
        , templateFormatLatex
        , templateFormatDocx
        , templateFormatOdt
        , templateFormatMarkdown
        ]
    }

module Shared.Database.Migration.Development.Template.Data.Templates where

import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Constant.Template
import Shared.Database.Migration.Development.Template.Data.DefaultTemplate (css, html)
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateGroup
import Shared.Util.Uuid

commonWizardTemplate :: Template
commonWizardTemplate =
  Template
    { _templateTId = "global:questionnaire-report:1.0.0"
    , _templateName = "Questionnaire Report"
    , _templateOrganizationId = "global"
    , _templateTemplateId = "questionnaire-report"
    , _templateVersion = "1.0.0"
    , _templateMetamodelVersion = templateMetamodelVersion
    , _templateDescription = "Exported questions and answers from a questionnaire"
    , _templateReadme = "# Default Template"
    , _templateLicense = "Apache-2.0"
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
    , _templateCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

commonWizardTemplateEdited :: Template
commonWizardTemplateEdited =
  commonWizardTemplate
    { _templateName = "EDITED: " ++ commonWizardTemplate ^. name
    , _templateDescription = "EDITED: " ++ commonWizardTemplate ^. description
    , _templateAllowedPackages = [templateAllowedPackageEdited]
    }

commonWizardTemplateGroup :: TemplateGroup
commonWizardTemplateGroup =
  TemplateGroup
    { _templateGroupOrganizationId = commonWizardTemplate ^. tId
    , _templateGroupTemplateId = commonWizardTemplate ^. templateId
    , _templateGroupVersions = [commonWizardTemplate]
    }

templateAllowedPackage :: TemplateAllowedPackage
templateAllowedPackage =
  TemplateAllowedPackage
    { _templateAllowedPackageOrgId = Nothing
    , _templateAllowedPackageKmId = Nothing
    , _templateAllowedPackageMinVersion = Nothing
    , _templateAllowedPackageMaxVersion = Nothing
    }

templateAllowedPackageEdited :: TemplateAllowedPackage
templateAllowedPackageEdited =
  TemplateAllowedPackage
    { _templateAllowedPackageOrgId = Just "global"
    , _templateAllowedPackageKmId = Nothing
    , _templateAllowedPackageMinVersion = Nothing
    , _templateAllowedPackageMaxVersion = Nothing
    }

templateFormatJson :: TemplateFormat
templateFormatJson =
  TemplateFormat
    { _templateFormatUuid = u' "d3e98eb6-344d-481f-8e37-6a67b6cd1ad2"
    , _templateFormatName = "JSON Data"
    , _templateFormatShortName = "json"
    , _templateFormatIcon = "far fa-file"
    , _templateFormatColor = "#f15a24"
    , _templateFormatSteps = [templateFormatJsonStep]
    }

templateFormatJsonStep :: TemplateFormatStep
templateFormatJsonStep = TemplateFormatStep {_templateFormatStepName = "json", _templateFormatStepOptions = M.empty}

templateFormatHtml :: TemplateFormat
templateFormatHtml =
  TemplateFormat
    { _templateFormatUuid = u' "a9293d08-59a4-4e6b-ae62-7a6a570b031c"
    , _templateFormatName = "HTML Document"
    , _templateFormatShortName = "html"
    , _templateFormatIcon = "far fa-file-code"
    , _templateFormatColor = "#f15a24"
    , _templateFormatSteps =
        [ TemplateFormatStep
            { _templateFormatStepName = "jinja"
            , _templateFormatStepOptions =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        ]
    }

templateFormatPdf :: TemplateFormat
templateFormatPdf =
  TemplateFormat
    { _templateFormatUuid = u' "68c26e34-5e77-4e15-9bf7-06ff92582257"
    , _templateFormatName = "PDF Document"
    , _templateFormatShortName = "pdf"
    , _templateFormatIcon = "far fa-file-pdf"
    , _templateFormatColor = "#f15a24"
    , _templateFormatSteps =
        [ TemplateFormatStep
            { _templateFormatStepName = "jinja"
            , _templateFormatStepOptions =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        , TemplateFormatStep {_templateFormatStepName = "wkhtmltopdf", _templateFormatStepOptions = M.empty}
        ]
    }

templateFormatLatex :: TemplateFormat
templateFormatLatex =
  TemplateFormat
    { _templateFormatUuid = u' "dbc94579-40d7-42c3-975c-71e30d07778b"
    , _templateFormatName = "LaTeX Document"
    , _templateFormatShortName = "latex"
    , _templateFormatIcon = "far fa-file-alt"
    , _templateFormatColor = "#f15a24"
    , _templateFormatSteps =
        [ TemplateFormatStep
            { _templateFormatStepName = "jinja"
            , _templateFormatStepOptions =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        , TemplateFormatStep
            { _templateFormatStepName = "pandoc"
            , _templateFormatStepOptions = M.fromList [("from", "html"), ("to", "latex")]
            }
        ]
    }

templateFormatDocx :: TemplateFormat
templateFormatDocx =
  TemplateFormat
    { _templateFormatUuid = u' "f4bd941a-dfbe-4226-a1fc-200fb5269311"
    , _templateFormatName = "MS Word Document"
    , _templateFormatShortName = "docx"
    , _templateFormatIcon = "far fa-file-word"
    , _templateFormatColor = "#f15a24"
    , _templateFormatSteps =
        [ TemplateFormatStep
            { _templateFormatStepName = "jinja"
            , _templateFormatStepOptions =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        , TemplateFormatStep
            { _templateFormatStepName = "pandoc"
            , _templateFormatStepOptions = M.fromList [("from", "html"), ("to", "docx")]
            }
        ]
    }

templateFormatOdt :: TemplateFormat
templateFormatOdt =
  TemplateFormat
    { _templateFormatUuid = u' "15e53172-bbae-4a0c-a4d9-8f3ddf60e7b6"
    , _templateFormatName = "OpenDocument Text"
    , _templateFormatShortName = "odt"
    , _templateFormatIcon = "far fa-file-alt"
    , _templateFormatColor = "#f15a24"
    , _templateFormatSteps =
        [ TemplateFormatStep
            { _templateFormatStepName = "jinja"
            , _templateFormatStepOptions =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        , TemplateFormatStep
            { _templateFormatStepName = "pandoc"
            , _templateFormatStepOptions = M.fromList [("from", "html"), ("to", "odt")]
            }
        ]
    }

templateFormatMarkdown :: TemplateFormat
templateFormatMarkdown =
  TemplateFormat
    { _templateFormatUuid = u' "f0533e48-f4c5-4af2-b2c1-5a47d4a247c0"
    , _templateFormatName = "Markdown Document"
    , _templateFormatShortName = "md"
    , _templateFormatIcon = "far fa-file-alt"
    , _templateFormatColor = "#f15a24"
    , _templateFormatSteps =
        [ TemplateFormatStep
            { _templateFormatStepName = "jinja"
            , _templateFormatStepOptions =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        , TemplateFormatStep
            { _templateFormatStepName = "pandoc"
            , _templateFormatStepOptions = M.fromList [("from", "html"), ("to", "markdown")]
            }
        ]
    }

templateFileDefaultHtml :: TemplateFile
templateFileDefaultHtml =
  TemplateFile
    { _templateFileTemplateId = commonWizardTemplate ^. tId
    , _templateFileUuid = u' "7f83f7ce-4096-49a5-88d1-bd509bf72a9b"
    , _templateFileFileName = "default.html.j2"
    , _templateFileContent = html
    }

templateFileDefaultHtmlEdited :: TemplateFile
templateFileDefaultHtmlEdited =
  templateFileDefaultHtml {_templateFileFileName = "default-edited.html.j2", _templateFileContent = "some new content"}

templateFileDefaultCss :: TemplateFile
templateFileDefaultCss =
  TemplateFile
    { _templateFileTemplateId = commonWizardTemplate ^. tId
    , _templateFileUuid = u' "ae41aa74-9605-4dfb-b1f9-b6064adc1dbc"
    , _templateFileFileName = "default.css"
    , _templateFileContent = css
    }

templateFileNewFile :: TemplateFile
templateFileNewFile =
  TemplateFile
    { _templateFileTemplateId = commonWizardTemplate ^. tId
    , _templateFileUuid = u' "63279989-d9ac-49da-81d3-fc8a56a8aa62"
    , _templateFileFileName = "new-file.txt"
    , _templateFileContent = "some content"
    }

templateAssetLogo :: TemplateAsset
templateAssetLogo =
  TemplateAsset
    { _templateAssetTemplateId = commonWizardTemplate ^. tId
    , _templateAssetUuid = u' "6c367648-9b60-4307-93b2-0851938adee0"
    , _templateAssetFileName = "text.txt"
    , _templateAssetContentType = "text/plain"
    }

templateAssetLogoContent :: BS.ByteString
templateAssetLogoContent =
  BS.pack $
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam auctor pellentesque velit, sollicitudin euismod " ++
  "arcu varius a. Pellentesque consectetur a felis nec finibus. Curabitur at porttitor turpis. Vivamus eu imperdiet " ++
  "massa. Fusce vitae dolor et nulla vulputate condimentum. Aenean tincidunt, magna quis viverra porta, nulla " ++
  "mauris semper nibh, ac interdum quam orci at elit. Donec aliquet tempor erat, sed consectetur sapien eleifend " ++
  "id. Nullam sagittis justo a lobortis fermentum. Nunc pretium sem sed lectus lacinia, et tempus nulla suscipit. " ++
  "Aliquam volutpat molestie nibh sit amet iaculis."

anotherWizardTemplate :: Template
anotherWizardTemplate =
  Template
    { _templateTId = "dsw:another-temlate:1.0.0"
    , _templateName = "Another Template"
    , _templateOrganizationId = "dsw"
    , _templateTemplateId = "another-template"
    , _templateVersion = "1.0.0"
    , _templateMetamodelVersion = templateMetamodelVersion
    , _templateDescription = "This is a another template"
    , _templateReadme = "# Another Template"
    , _templateLicense = "Apache-2.0"
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
    , _templateCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

anotherFileHtml :: TemplateFile
anotherFileHtml =
  TemplateFile
    { _templateFileTemplateId = anotherWizardTemplate ^. tId
    , _templateFileUuid = u' "7444f722-4972-4bf8-86d8-d4f01875572d"
    , _templateFileFileName = "default.html.j2"
    , _templateFileContent = html
    }

anotherFileCss :: TemplateFile
anotherFileCss =
  TemplateFile
    { _templateFileTemplateId = anotherWizardTemplate ^. tId
    , _templateFileUuid = u' "ac60ddb8-4561-4d4b-8d85-c8446bc96b56"
    , _templateFileFileName = "default.css"
    , _templateFileContent = css
    }

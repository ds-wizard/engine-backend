module Shared.Database.Migration.Development.Template.Data.Templates where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time

import Shared.Constant.App
import Shared.Constant.Template
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.Template.Data.DefaultTemplate (css, html)
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateGroup
import Shared.Util.Uuid

commonWizardTemplate :: Template
commonWizardTemplate =
  Template
    { tId = "global:questionnaire-report:1.0.0"
    , name = "Questionnaire Report"
    , organizationId = "global"
    , templateId = "questionnaire-report"
    , version = "1.0.0"
    , metamodelVersion = templateMetamodelVersion
    , description = "Exported questions and answers from a questionnaire"
    , readme = "# Default Template"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternAll]
    , recommendedPackageId = Nothing
    , formats =
        [ templateFormatJson
        , templateFormatHtml
        , templateFormatPdf
        , templateFormatLatex
        , templateFormatDocx
        , templateFormatOdt
        , templateFormatMarkdown
        ]
    , appUuid = defaultAppUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

commonWizardTemplateEdited :: Template
commonWizardTemplateEdited =
  commonWizardTemplate
    { name = "EDITED: " ++ commonWizardTemplate.name
    , description = "EDITED: " ++ commonWizardTemplate.description
    , allowedPackages = [packagePatternAllEdited]
    }

commonWizardTemplateGroup :: TemplateGroup
commonWizardTemplateGroup =
  TemplateGroup
    { organizationId = commonWizardTemplate.tId
    , templateId = commonWizardTemplate.templateId
    , versions = [commonWizardTemplate]
    }

templateFormatJson :: TemplateFormat
templateFormatJson =
  TemplateFormat
    { uuid = u' "d3e98eb6-344d-481f-8e37-6a67b6cd1ad2"
    , name = "JSON Data"
    , shortName = "json"
    , icon = "far fa-file"
    , color = "#f15a24"
    , steps = [templateFormatJsonStep]
    }

templateFormatJsonStep :: TemplateFormatStep
templateFormatJsonStep = TemplateFormatStep {name = "json", options = M.empty}

templateFormatHtml :: TemplateFormat
templateFormatHtml =
  TemplateFormat
    { uuid = u' "a9293d08-59a4-4e6b-ae62-7a6a570b031c"
    , name = "HTML Document"
    , shortName = "html"
    , icon = "far fa-file-code"
    , color = "#f15a24"
    , steps =
        [ TemplateFormatStep
            { name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        ]
    }

templateFormatPdf :: TemplateFormat
templateFormatPdf =
  TemplateFormat
    { uuid = u' "68c26e34-5e77-4e15-9bf7-06ff92582257"
    , name = "PDF Document"
    , shortName = "pdf"
    , icon = "far fa-file-pdf"
    , color = "#f15a24"
    , steps =
        [ TemplateFormatStep
            { name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        , TemplateFormatStep {name = "wkhtmltopdf", options = M.empty}
        ]
    }

templateFormatLatex :: TemplateFormat
templateFormatLatex =
  TemplateFormat
    { uuid = u' "dbc94579-40d7-42c3-975c-71e30d07778b"
    , name = "LaTeX Document"
    , shortName = "latex"
    , icon = "far fa-file-alt"
    , color = "#f15a24"
    , steps =
        [ TemplateFormatStep
            { name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        , TemplateFormatStep
            { name = "pandoc"
            , options = M.fromList [("from", "html"), ("to", "latex")]
            }
        ]
    }

templateFormatDocx :: TemplateFormat
templateFormatDocx =
  TemplateFormat
    { uuid = u' "f4bd941a-dfbe-4226-a1fc-200fb5269311"
    , name = "MS Word Document"
    , shortName = "docx"
    , icon = "far fa-file-word"
    , color = "#f15a24"
    , steps =
        [ TemplateFormatStep
            { name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        , TemplateFormatStep
            { name = "pandoc"
            , options = M.fromList [("from", "html"), ("to", "docx")]
            }
        ]
    }

templateFormatOdt :: TemplateFormat
templateFormatOdt =
  TemplateFormat
    { uuid = u' "15e53172-bbae-4a0c-a4d9-8f3ddf60e7b6"
    , name = "OpenDocument Text"
    , shortName = "odt"
    , icon = "far fa-file-alt"
    , color = "#f15a24"
    , steps =
        [ TemplateFormatStep
            { name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        , TemplateFormatStep
            { name = "pandoc"
            , options = M.fromList [("from", "html"), ("to", "odt")]
            }
        ]
    }

templateFormatMarkdown :: TemplateFormat
templateFormatMarkdown =
  TemplateFormat
    { uuid = u' "f0533e48-f4c5-4af2-b2c1-5a47d4a247c0"
    , name = "Markdown Document"
    , shortName = "md"
    , icon = "far fa-file-alt"
    , color = "#f15a24"
    , steps =
        [ TemplateFormatStep
            { name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        , TemplateFormatStep
            { name = "pandoc"
            , options = M.fromList [("from", "html"), ("to", "markdown")]
            }
        ]
    }

templateFileDefaultHtml :: TemplateFile
templateFileDefaultHtml =
  TemplateFile
    { templateId = commonWizardTemplate.tId
    , uuid = u' "7f83f7ce-4096-49a5-88d1-bd509bf72a9b"
    , fileName = "default.html.j2"
    , content = html
    , appUuid = defaultAppUuid
    }

templateFileDefaultHtmlEdited :: TemplateFile
templateFileDefaultHtmlEdited =
  templateFileDefaultHtml {fileName = "default-edited.html.j2", content = "some new content"}

templateFileDefaultCss :: TemplateFile
templateFileDefaultCss =
  TemplateFile
    { templateId = commonWizardTemplate.tId
    , uuid = u' "ae41aa74-9605-4dfb-b1f9-b6064adc1dbc"
    , fileName = "default.css"
    , content = css
    , appUuid = defaultAppUuid
    }

templateFileNewFile :: TemplateFile
templateFileNewFile =
  TemplateFile
    { templateId = commonWizardTemplate.tId
    , uuid = u' "63279989-d9ac-49da-81d3-fc8a56a8aa62"
    , fileName = "new-file.txt"
    , content = "some content"
    , appUuid = defaultAppUuid
    }

templateAssetLogo :: TemplateAsset
templateAssetLogo =
  TemplateAsset
    { templateId = commonWizardTemplate.tId
    , uuid = u' "6c367648-9b60-4307-93b2-0851938adee0"
    , fileName = "text.txt"
    , contentType = "text/plain"
    , fileSize = 5 * 1024
    , appUuid = defaultAppUuid
    }

templateAssetLogoContent :: BS.ByteString
templateAssetLogoContent =
  BS.pack $
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam auctor pellentesque velit, sollicitudin euismod "
      ++ "arcu varius a. Pellentesque consectetur a felis nec finibus. Curabitur at porttitor turpis. Vivamus eu imperdiet "
      ++ "massa. Fusce vitae dolor et nulla vulputate condimentum. Aenean tincidunt, magna quis viverra porta, nulla "
      ++ "mauris semper nibh, ac interdum quam orci at elit. Donec aliquet tempor erat, sed consectetur sapien eleifend "
      ++ "id. Nullam sagittis justo a lobortis fermentum. Nunc pretium sem sed lectus lacinia, et tempus nulla suscipit. "
      ++ "Aliquam volutpat molestie nibh sit amet iaculis."

anotherWizardTemplate :: Template
anotherWizardTemplate =
  Template
    { tId = "dsw:another-temlate:1.0.0"
    , name = "Another Template"
    , organizationId = "dsw"
    , templateId = "another-template"
    , version = "1.0.0"
    , metamodelVersion = templateMetamodelVersion
    , description = "This is a another template"
    , readme = "# Another Template"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternAll]
    , recommendedPackageId = Nothing
    , formats =
        [ templateFormatJson
        , templateFormatHtml
        , templateFormatPdf
        , templateFormatLatex
        , templateFormatDocx
        , templateFormatOdt
        , templateFormatMarkdown
        ]
    , appUuid = defaultAppUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

anotherFileHtml :: TemplateFile
anotherFileHtml =
  TemplateFile
    { templateId = anotherWizardTemplate.tId
    , uuid = u' "7444f722-4972-4bf8-86d8-d4f01875572d"
    , fileName = "default.html.j2"
    , content = html
    , appUuid = defaultAppUuid
    }

anotherFileCss :: TemplateFile
anotherFileCss =
  TemplateFile
    { templateId = anotherWizardTemplate.tId
    , uuid = u' "ac60ddb8-4561-4d4b-8d85-c8446bc96b56"
    , fileName = "default.css"
    , content = css
    , appUuid = defaultAppUuid
    }

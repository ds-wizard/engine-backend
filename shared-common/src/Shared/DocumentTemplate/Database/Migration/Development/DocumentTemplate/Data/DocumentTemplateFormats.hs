module Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats where

import qualified Data.Map.Strict as M

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateUtil

wizardDocumentTemplateFormats :: [DocumentTemplateFormat]
wizardDocumentTemplateFormats =
  [ formatHtml
  , formatJson
  , formatLatex
  , formatMarkdown
  , formatOdt
  , formatPdf
  , formatDocx
  ]

wizardDocumentTemplateDraftFormats :: [DocumentTemplateFormat]
wizardDocumentTemplateDraftFormats = changeDocumentTemplateIdInFormats wizardDocumentTemplateDraft.tId wizardDocumentTemplateDraft.tenantUuid wizardDocumentTemplateFormats

differentDocumentTemplateFormats :: [DocumentTemplateFormat]
differentDocumentTemplateFormats = changeDocumentTemplateIdInFormats differentDocumentTemplate.tId differentDocumentTemplate.tenantUuid wizardDocumentTemplateFormats

formatJson :: DocumentTemplateFormat
formatJson =
  DocumentTemplateFormat
    { documentTemplateId = wizardDocumentTemplate.tId
    , uuid = u' "d3e98eb6-344d-481f-8e37-6a67b6cd1ad2"
    , name = "JSON Data"
    , icon = "far fa-file"
    , steps = [formatJsonStep]
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

formatJsonSimple :: DocumentTemplateFormatSimple
formatJsonSimple = toFormatSimple formatJson

formatJsonStep :: DocumentTemplateFormatStep
formatJsonStep =
  DocumentTemplateFormatStep
    { documentTemplateId = wizardDocumentTemplate.tId
    , formatUuid = u' "d3e98eb6-344d-481f-8e37-6a67b6cd1ad2"
    , position = 0
    , name = "json"
    , options = M.empty
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

formatHtml :: DocumentTemplateFormat
formatHtml =
  DocumentTemplateFormat
    { documentTemplateId = wizardDocumentTemplate.tId
    , uuid = u' "a9293d08-59a4-4e6b-ae62-7a6a570b031c"
    , name = "HTML Document"
    , icon = "far fa-file-code"
    , steps =
        [ DocumentTemplateFormatStep
            { documentTemplateId = wizardDocumentTemplate.tId
            , formatUuid = u' "a9293d08-59a4-4e6b-ae62-7a6a570b031c"
            , position = 0
            , name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            , tenantUuid = defaultTenantUuid
            , createdAt = dt' 2018 1 21
            , updatedAt = dt' 2018 1 21
            }
        ]
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

formatPdf :: DocumentTemplateFormat
formatPdf =
  DocumentTemplateFormat
    { documentTemplateId = wizardDocumentTemplate.tId
    , uuid = u' "68c26e34-5e77-4e15-9bf7-06ff92582257"
    , name = "PDF Document"
    , icon = "far fa-file-pdf"
    , steps =
        [ DocumentTemplateFormatStep
            { documentTemplateId = wizardDocumentTemplate.tId
            , formatUuid = u' "68c26e34-5e77-4e15-9bf7-06ff92582257"
            , position = 0
            , name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            , tenantUuid = defaultTenantUuid
            , createdAt = dt' 2018 1 21
            , updatedAt = dt' 2018 1 21
            }
        , DocumentTemplateFormatStep
            { documentTemplateId = wizardDocumentTemplate.tId
            , formatUuid = u' "68c26e34-5e77-4e15-9bf7-06ff92582257"
            , position = 1
            , name = "wkhtmltopdf"
            , options = M.empty
            , tenantUuid = defaultTenantUuid
            , createdAt = dt' 2018 1 21
            , updatedAt = dt' 2018 1 21
            }
        ]
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

formatLatex :: DocumentTemplateFormat
formatLatex =
  DocumentTemplateFormat
    { documentTemplateId = wizardDocumentTemplate.tId
    , uuid = u' "dbc94579-40d7-42c3-975c-71e30d07778b"
    , name = "LaTeX Document"
    , icon = "far fa-file-alt"
    , steps =
        [ DocumentTemplateFormatStep
            { documentTemplateId = wizardDocumentTemplate.tId
            , formatUuid = u' "dbc94579-40d7-42c3-975c-71e30d07778b"
            , position = 0
            , name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            , tenantUuid = defaultTenantUuid
            , createdAt = dt' 2018 1 21
            , updatedAt = dt' 2018 1 21
            }
        , DocumentTemplateFormatStep
            { documentTemplateId = wizardDocumentTemplate.tId
            , formatUuid = u' "dbc94579-40d7-42c3-975c-71e30d07778b"
            , position = 1
            , name = "pandoc"
            , options = M.fromList [("from", "html"), ("to", "latex")]
            , tenantUuid = defaultTenantUuid
            , createdAt = dt' 2018 1 21
            , updatedAt = dt' 2018 1 21
            }
        ]
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

formatDocx :: DocumentTemplateFormat
formatDocx =
  DocumentTemplateFormat
    { documentTemplateId = wizardDocumentTemplate.tId
    , uuid = u' "f4bd941a-dfbe-4226-a1fc-200fb5269311"
    , name = "Word Document"
    , icon = "far fa-file-word"
    , steps =
        [ DocumentTemplateFormatStep
            { documentTemplateId = wizardDocumentTemplate.tId
            , formatUuid = u' "f4bd941a-dfbe-4226-a1fc-200fb5269311"
            , position = 0
            , name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            , tenantUuid = defaultTenantUuid
            , createdAt = dt' 2018 1 21
            , updatedAt = dt' 2018 1 21
            }
        , DocumentTemplateFormatStep
            { documentTemplateId = wizardDocumentTemplate.tId
            , formatUuid = u' "f4bd941a-dfbe-4226-a1fc-200fb5269311"
            , position = 1
            , name = "pandoc"
            , options = M.fromList [("from", "html"), ("to", "docx")]
            , tenantUuid = defaultTenantUuid
            , createdAt = dt' 2018 1 21
            , updatedAt = dt' 2018 1 21
            }
        ]
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

formatOdt :: DocumentTemplateFormat
formatOdt =
  DocumentTemplateFormat
    { documentTemplateId = wizardDocumentTemplate.tId
    , uuid = u' "15e53172-bbae-4a0c-a4d9-8f3ddf60e7b6"
    , name = "OpenDocument Text"
    , icon = "far fa-file-alt"
    , steps =
        [ DocumentTemplateFormatStep
            { documentTemplateId = wizardDocumentTemplate.tId
            , formatUuid = u' "15e53172-bbae-4a0c-a4d9-8f3ddf60e7b6"
            , position = 0
            , name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            , tenantUuid = defaultTenantUuid
            , createdAt = dt' 2018 1 21
            , updatedAt = dt' 2018 1 21
            }
        , DocumentTemplateFormatStep
            { documentTemplateId = wizardDocumentTemplate.tId
            , formatUuid = u' "15e53172-bbae-4a0c-a4d9-8f3ddf60e7b6"
            , position = 1
            , name = "pandoc"
            , options = M.fromList [("from", "html"), ("to", "odt")]
            , tenantUuid = defaultTenantUuid
            , createdAt = dt' 2018 1 21
            , updatedAt = dt' 2018 1 21
            }
        ]
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

formatMarkdown :: DocumentTemplateFormat
formatMarkdown =
  DocumentTemplateFormat
    { documentTemplateId = wizardDocumentTemplate.tId
    , uuid = u' "f0533e48-f4c5-4af2-b2c1-5a47d4a247c0"
    , name = "Markdown Document"
    , icon = "far fa-file-alt"
    , steps =
        [ DocumentTemplateFormatStep
            { documentTemplateId = wizardDocumentTemplate.tId
            , formatUuid = u' "f0533e48-f4c5-4af2-b2c1-5a47d4a247c0"
            , position = 0
            , name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            , tenantUuid = defaultTenantUuid
            , createdAt = dt' 2018 1 21
            , updatedAt = dt' 2018 1 21
            }
        , DocumentTemplateFormatStep
            { documentTemplateId = wizardDocumentTemplate.tId
            , formatUuid = u' "f0533e48-f4c5-4af2-b2c1-5a47d4a247c0"
            , position = 1
            , name = "pandoc"
            , options = M.fromList [("from", "html"), ("to", "markdown")]
            , tenantUuid = defaultTenantUuid
            , createdAt = dt' 2018 1 21
            , updatedAt = dt' 2018 1 21
            }
        ]
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

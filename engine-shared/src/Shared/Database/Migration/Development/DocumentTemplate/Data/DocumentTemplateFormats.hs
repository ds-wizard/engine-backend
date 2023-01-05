module Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats where

import qualified Data.Map.Strict as M

import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Util.Uuid

formatJson :: DocumentTemplateFormat
formatJson =
  DocumentTemplateFormat
    { uuid = u' "d3e98eb6-344d-481f-8e37-6a67b6cd1ad2"
    , name = "JSON Data"
    , icon = "far fa-file"
    , steps = [formatJsonStep]
    }

formatJsonStep :: DocumentTemplateFormatStep
formatJsonStep = DocumentTemplateFormatStep {name = "json", options = M.empty}

formatHtml :: DocumentTemplateFormat
formatHtml =
  DocumentTemplateFormat
    { uuid = u' "a9293d08-59a4-4e6b-ae62-7a6a570b031c"
    , name = "HTML Document"
    , icon = "far fa-file-code"
    , steps =
        [ DocumentTemplateFormatStep
            { name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        ]
    }

formatPdf :: DocumentTemplateFormat
formatPdf =
  DocumentTemplateFormat
    { uuid = u' "68c26e34-5e77-4e15-9bf7-06ff92582257"
    , name = "PDF Document"
    , icon = "far fa-file-pdf"
    , steps =
        [ DocumentTemplateFormatStep
            { name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        , DocumentTemplateFormatStep {name = "wkhtmltopdf", options = M.empty}
        ]
    }

formatLatex :: DocumentTemplateFormat
formatLatex =
  DocumentTemplateFormat
    { uuid = u' "dbc94579-40d7-42c3-975c-71e30d07778b"
    , name = "LaTeX Document"
    , icon = "far fa-file-alt"
    , steps =
        [ DocumentTemplateFormatStep
            { name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        , DocumentTemplateFormatStep
            { name = "pandoc"
            , options = M.fromList [("from", "html"), ("to", "latex")]
            }
        ]
    }

formatDocx :: DocumentTemplateFormat
formatDocx =
  DocumentTemplateFormat
    { uuid = u' "f4bd941a-dfbe-4226-a1fc-200fb5269311"
    , name = "MS Word Document"
    , icon = "far fa-file-word"
    , steps =
        [ DocumentTemplateFormatStep
            { name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        , DocumentTemplateFormatStep
            { name = "pandoc"
            , options = M.fromList [("from", "html"), ("to", "docx")]
            }
        ]
    }

formatOdt :: DocumentTemplateFormat
formatOdt =
  DocumentTemplateFormat
    { uuid = u' "15e53172-bbae-4a0c-a4d9-8f3ddf60e7b6"
    , name = "OpenDocument Text"
    , icon = "far fa-file-alt"
    , steps =
        [ DocumentTemplateFormatStep
            { name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        , DocumentTemplateFormatStep
            { name = "pandoc"
            , options = M.fromList [("from", "html"), ("to", "odt")]
            }
        ]
    }

formatMarkdown :: DocumentTemplateFormat
formatMarkdown =
  DocumentTemplateFormat
    { uuid = u' "f0533e48-f4c5-4af2-b2c1-5a47d4a247c0"
    , name = "Markdown Document"
    , icon = "far fa-file-alt"
    , steps =
        [ DocumentTemplateFormatStep
            { name = "jinja"
            , options =
                M.fromList [("template", "default.html.j2"), ("content-type", "text/html"), ("extension", "html")]
            }
        , DocumentTemplateFormatStep
            { name = "pandoc"
            , options = M.fromList [("from", "html"), ("to", "markdown")]
            }
        ]
    }

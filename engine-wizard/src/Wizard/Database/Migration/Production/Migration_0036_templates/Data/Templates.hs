module Wizard.Database.Migration.Production.Migration_0036_templates.Data.Templates where

import qualified Data.Bson as BSON

import Wizard.Database.Migration.Production.Migration_0036_templates.Data.DefaultTemplate (css, html)

template now =
  [ "id" BSON.=: "dsw:default:1.0.0"
  , "name" BSON.=: "Default Template"
  , "organizationId" BSON.=: "dsw"
  , "templateId" BSON.=: "default"
  , "version" BSON.=: "1.0.0"
  , "metamodelVersion" BSON.=: 1
  , "description" BSON.=: "This is a default template"
  , "readme" BSON.=: "# Default Template"
  , "license" BSON.=: "Apache-2.0"
  , "allowedPackages" BSON.=:
    [ [ "orgId" BSON.=: (Nothing :: Maybe String)
      , "kmId" BSON.=: (Nothing :: Maybe String)
      , "minVersion" BSON.=: (Nothing :: Maybe String)
      , "maxVersion" BSON.=: (Nothing :: Maybe String)
      ]
    ]
  , "recommendedPackageId" BSON.=: (Nothing :: Maybe String)
  , "formats" BSON.=:
    [ [ "uuid" BSON.=: "d3e98eb6-344d-481f-8e37-6a67b6cd1ad2"
      , "name" BSON.=: "JSON Data"
      , "shortName" BSON.=: "json"
      , "icon" BSON.=: "far fa-file"
      , "color" BSON.=: "#f15a24"
      , "steps" BSON.=: [["name" BSON.=: "json", "options" BSON.=: ([] :: BSON.Document)]]
      ]
    , [ "uuid" BSON.=: "a9293d08-59a4-4e6b-ae62-7a6a570b031c"
      , "name" BSON.=: "HTML Document"
      , "shortName" BSON.=: "html"
      , "icon" BSON.=: "far fa-file-code"
      , "color" BSON.=: "#f15a24"
      , "steps" BSON.=:
        [ [ "name" BSON.=: "jinja"
          , "options" BSON.=:
            ["template" BSON.=: "default.html.j2", "content-type" BSON.=: "text/html", "extension" BSON.=: "html"]
          ]
        ]
      ]
    , [ "uuid" BSON.=: "68c26e34-5e77-4e15-9bf7-06ff92582257"
      , "name" BSON.=: "PDF Document"
      , "shortName" BSON.=: "pdf"
      , "icon" BSON.=: "far fa-file-pdf"
      , "color" BSON.=: "#f15a24"
      , "steps" BSON.=:
        [ [ "name" BSON.=: "jinja"
          , "options" BSON.=:
            ["template" BSON.=: "default.html.j2", "content-type" BSON.=: "text/html", "extension" BSON.=: "html"]
          ]
        , ["name" BSON.=: "wkhtmltopdf", "options" BSON.=: ([] :: BSON.Document)]
        ]
      ]
    , [ "uuid" BSON.=: "dbc94579-40d7-42c3-975c-71e30d07778b"
      , "name" BSON.=: "LaTeX Document"
      , "shortName" BSON.=: "latex"
      , "icon" BSON.=: "far fa-file-alt"
      , "color" BSON.=: "#f15a24"
      , "steps" BSON.=:
        [ [ "name" BSON.=: "jinja"
          , "options" BSON.=:
            ["template" BSON.=: "default.html.j2", "content-type" BSON.=: "text/html", "extension" BSON.=: "html"]
          ]
        , ["name" BSON.=: "pandoc", "options" BSON.=: ["from" BSON.=: "html", "to" BSON.=: "latex"]]
        ]
      ]
    , [ "uuid" BSON.=: "f4bd941a-dfbe-4226-a1fc-200fb5269311"
      , "name" BSON.=: "MS Word Document"
      , "shortName" BSON.=: "docx"
      , "icon" BSON.=: "far fa-file-word"
      , "color" BSON.=: "#f15a24"
      , "steps" BSON.=:
        [ [ "name" BSON.=: "jinja"
          , "options" BSON.=:
            ["template" BSON.=: "default.html.j2", "content-type" BSON.=: "text/html", "extension" BSON.=: "html"]
          ]
        , ["name" BSON.=: "pandoc", "options" BSON.=: ["from" BSON.=: "html", "to" BSON.=: "docx"]]
        ]
      ]
    , [ "uuid" BSON.=: "15e53172-bbae-4a0c-a4d9-8f3ddf60e7b6"
      , "name" BSON.=: "OpenDocument Text"
      , "shortName" BSON.=: "odt"
      , "icon" BSON.=: "far fa-file-alt"
      , "color" BSON.=: "#f15a24"
      , "steps" BSON.=:
        [ [ "name" BSON.=: "jinja"
          , "options" BSON.=:
            ["template" BSON.=: "default.html.j2", "content-type" BSON.=: "text/html", "extension" BSON.=: "html"]
          ]
        , ["name" BSON.=: "pandoc", "options" BSON.=: ["from" BSON.=: "html", "to" BSON.=: "odt"]]
        ]
      ]
    , [ "uuid" BSON.=: "f0533e48-f4c5-4af2-b2c1-5a47d4a247c0"
      , "name" BSON.=: "Markdown Document"
      , "shortName" BSON.=: "md"
      , "icon" BSON.=: "far fa-file-alt"
      , "color" BSON.=: "#f15a24"
      , "steps" BSON.=:
        [ [ "name" BSON.=: "jinja"
          , "options" BSON.=:
            ["template" BSON.=: "default.html.j2", "content-type" BSON.=: "text/html", "extension" BSON.=: "html"]
          ]
        , ["name" BSON.=: "pandoc", "options" BSON.=: ["from" BSON.=: "html", "to" BSON.=: "markdown"]]
        ]
      ]
    ]
  , "assets" BSON.=: ([] :: [String])
  , "files" BSON.=:
    [ [ "uuid" BSON.=: "7f83f7ce-4096-49a5-88d1-bd509bf72a9b"
      , "fileName" BSON.=: "default.html.j2"
      , "content" BSON.=: html
      ]
    , [ "uuid" BSON.=: "ae41aa74-9605-4dfb-b1f9-b6064adc1dbc"
      , "fileName" BSON.=: "default.css"
      , "content" BSON.=: css
      ]
    ]
  , "createdAt" BSON.=: now
  ]

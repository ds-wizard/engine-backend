module Wizard.Database.Migration.Production.Migration_0026_add_application_configs.Data.Configs where

import qualified Data.Bson as BSON

config now =
  [ "organization" BSON.=: ["organizationId" BSON.=: "", "name" BSON.=: "", "affiliations" BSON.=: ([] :: [String])]
  , "authentication" BSON.=:
    [ "defaultRole" BSON.=: "dataSteward"
    , "internal" BSON.=: ["registration" BSON.=: ["enabled" BSON.=: True]]
    , "external" BSON.=: ["services" BSON.=: ([] :: [String])]
    ]
  , "privacyAndSupport" BSON.=:
    [ "privacyUrl" BSON.=: (Nothing :: Maybe String)
    , "supportEmail" BSON.=: (Nothing :: Maybe String)
    , "supportRepositoryName" BSON.=: (Nothing :: Maybe String)
    , "supportRepositoryUrl" BSON.=: (Nothing :: Maybe String)
    ]
  , "dashboard" BSON.=:
    [ "widgets" BSON.=: (Nothing :: Maybe [BSON.Document])
    , "welcomeWarning" BSON.=: (Nothing :: Maybe String)
    , "welcomeInfo" BSON.=: (Nothing :: Maybe String)
    ]
  , "lookAndFeel" BSON.=:
    [ "appTitle" BSON.=: (Nothing :: Maybe String)
    , "appTitleShort" BSON.=: (Nothing :: Maybe String)
    , "customMenuLinks" BSON.=: ([] :: [String])
    , "loginInfo" BSON.=: (Nothing :: Maybe String)
    ]
  , "knowledgeModelRegistry" BSON.=: ["enabled" BSON.=: False, "token" BSON.=: ""]
  , "questionnaire" BSON.=:
    [ "publicQuestionnaire" BSON.=: ["enabled" BSON.=: False]
    , "levels" BSON.=: ["enabled" BSON.=: True]
    , "feedback" BSON.=: ["enabled" BSON.=: False, "token" BSON.=: "", "owner" BSON.=: "", "repo" BSON.=: ""]
    , "questionnaireAccessibility" BSON.=: ["enabled" BSON.=: True]
    ]
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

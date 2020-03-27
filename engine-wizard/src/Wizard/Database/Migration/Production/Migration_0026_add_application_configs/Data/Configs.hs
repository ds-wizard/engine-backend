module Wizard.Database.Migration.Production.Migration_0026_add_application_configs.Data.Configs where

import qualified Data.Bson as BSON

config now =
  [ "features" BSON.=:
    [ "publicQuestionnaire" BSON.=: ["enabled" BSON.=: False]
    , "levels" BSON.=: ["enabled" BSON.=: True]
    , "questionnaireAccessibility" BSON.=: ["enabled" BSON.=: True]
    ]
  , "client" BSON.=:
    [ "privacyUrl" BSON.=: "https://ds-wizard.org/privacy.html"
    , "appTitle" BSON.=: (Nothing :: Maybe String)
    , "appTitleShort" BSON.=: (Nothing :: Maybe String)
    , "supportEmail" BSON.=: (Nothing :: Maybe String)
    , "supportRepositoryName" BSON.=: (Nothing :: Maybe String)
    , "supportRepositoryUrl" BSON.=: (Nothing :: Maybe String)
    , "dashboard" BSON.=:
      Just ["admin" BSON.=: ["Welcome"], "dataSteward" BSON.=: ["Welcome"], "researcher" BSON.=: ["Welcome"]]
    , "customMenuLinks" BSON.=: ([] :: [String])
    ]
  , "info" BSON.=:
    [ "welcomeWarning" BSON.=: (Nothing :: Maybe String)
    , "welcomeInfo" BSON.=: (Nothing :: Maybe String)
    , "loginInfo" BSON.=: (Nothing :: Maybe String)
    ]
  , "affiliation" BSON.=: ["affiliations" BSON.=: ([] :: [String])]
  , "auth" BSON.=:
    [ "internal" BSON.=: ["registration" BSON.=: ["enabled" BSON.=: True]]
    , "external" BSON.=: ["services" BSON.=: ([] :: [String])]
    ]
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

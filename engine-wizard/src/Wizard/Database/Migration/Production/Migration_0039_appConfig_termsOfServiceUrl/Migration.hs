module Wizard.Database.Migration.Production.Migration_0039_appConfig_termsOfServiceUrl.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 39
    , mmName = "AppConfig PrivacyAndSupport - TermsOfService"
    , mmDescription = "Add 'summaryReport' to Qtn AppConfig"
    }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let action =
        modify (select [] "appConfigs") ["$set" =: ["privacyAndSupport.termsOfServiceUrl" =: (Nothing :: Maybe String)]]
  runMongoDBPoolDef action dbPool
  return Nothing

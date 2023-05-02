module Wizard.Database.Mapping.Limit.AppLimit where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Limit.AppLimit

instance FromRow AppLimit where
  fromRow = do
    uuid <- field
    users <- field
    activeUsers <- field
    knowledgeModels <- field
    branches <- field
    documentTemplates <- field
    questionnaires <- field
    documents <- field
    storage <- field
    createdAt <- field
    updatedAt <- field
    documentTemplateDrafts <- field
    locales <- field
    return $ AppLimit {..}

instance ToRow AppLimit where
  toRow AppLimit {..} =
    [ toField uuid
    , toField users
    , toField activeUsers
    , toField knowledgeModels
    , toField branches
    , toField documentTemplates
    , toField questionnaires
    , toField documents
    , toField storage
    , toField createdAt
    , toField updatedAt
    , toField documentTemplateDrafts
    , toField locales
    ]

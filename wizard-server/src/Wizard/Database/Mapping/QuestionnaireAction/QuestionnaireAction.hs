module Wizard.Database.Mapping.QuestionnaireAction.QuestionnaireAction where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternJM ()
import Wizard.Model.QuestionnaireAction.QuestionnaireAction

instance ToRow QuestionnaireAction where
  toRow QuestionnaireAction {..} =
    [ toField qaId
    , toField name
    , toField organizationId
    , toField actionId
    , toField version
    , toField metamodelVersion
    , toField description
    , toField readme
    , toField license
    , toJSONField allowedPackages
    , toField url
    , toJSONField config
    , toField enabled
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow QuestionnaireAction where
  fromRow = do
    qaId <- field
    name <- field
    organizationId <- field
    actionId <- field
    version <- field
    metamodelVersion <- field
    description <- field
    readme <- field
    license <- field
    allowedPackages <- fieldWith fromJSONField
    url <- field
    config <- fieldWith fromJSONField
    enabled <- field
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    return $ QuestionnaireAction {..}

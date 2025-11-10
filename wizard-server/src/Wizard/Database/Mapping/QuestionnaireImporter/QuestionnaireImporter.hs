module Wizard.Database.Mapping.QuestionnaireImporter.QuestionnaireImporter where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternJM ()
import Wizard.Model.QuestionnaireImporter.QuestionnaireImporter

instance ToRow QuestionnaireImporter where
  toRow QuestionnaireImporter {..} =
    [ toField qiId
    , toField name
    , toField organizationId
    , toField importerId
    , toField version
    , toField metamodelVersion
    , toField description
    , toField readme
    , toField license
    , toJSONField allowedPackages
    , toField url
    , toField enabled
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow QuestionnaireImporter where
  fromRow = do
    qiId <- field
    name <- field
    organizationId <- field
    importerId <- field
    version <- field
    metamodelVersion <- field
    description <- field
    readme <- field
    license <- field
    allowedPackages <- fieldWith fromJSONField
    url <- field
    enabled <- field
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    return $ QuestionnaireImporter {..}

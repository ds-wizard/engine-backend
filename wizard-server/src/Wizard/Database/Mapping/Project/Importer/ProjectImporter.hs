module Wizard.Database.Mapping.Project.Importer.ProjectImporter where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternJM ()
import Wizard.Model.Project.Importer.ProjectImporter

instance ToRow ProjectImporter where
  toRow ProjectImporter {..} =
    [ toField piId
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

instance FromRow ProjectImporter where
  fromRow = do
    piId <- field
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
    return $ ProjectImporter {..}

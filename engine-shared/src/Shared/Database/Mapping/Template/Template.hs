module Shared.Database.Mapping.Template.Template where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Api.Resource.Template.TemplateJM ()
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()

instance ToRow Template where
  toRow Template {..} =
    [ toField tId
    , toField name
    , toField organizationId
    , toField templateId
    , toField version
    , toField metamodelVersion
    , toField description
    , toField readme
    , toField license
    , toJSONField allowedPackages
    , toField recommendedPackageId
    , toJSONField formats
    , toField createdAt
    , toField appUuid
    ]

instance FromRow Template where
  fromRow = do
    tId <- field
    name <- field
    organizationId <- field
    templateId <- field
    version <- field
    metamodelVersion <- field
    description <- field
    readme <- field
    license <- field
    allowedPackages <- fieldWith fromJSONField
    recommendedPackageId <- field
    formats <- fieldWith fromJSONField
    createdAt <- field
    appUuid <- field
    return $ Template {..}

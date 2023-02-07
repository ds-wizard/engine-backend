module Shared.Database.Mapping.DocumentTemplate.DocumentTemplate where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplatePhase ()
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()

instance ToRow DocumentTemplate where
  toRow DocumentTemplate {..} =
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
    , toJSONField formats
    , toField createdAt
    , toField appUuid
    , toField updatedAt
    , toField phase
    ]

instance FromRow DocumentTemplate where
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
    formats <- fieldWith fromJSONField
    createdAt <- field
    appUuid <- field
    updatedAt <- field
    phase <- field
    return $ DocumentTemplate {..}

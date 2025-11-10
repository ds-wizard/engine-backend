module Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplate where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Common.Database.Mapping.Common.SemVer2Tuple ()
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplatePhase ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

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
    , toField createdAt
    , toField tenantUuid
    , toField updatedAt
    , toField phase
    , toField nonEditable
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
    createdAt <- field
    tenantUuid <- field
    updatedAt <- field
    phase <- field
    nonEditable <- field
    return $ DocumentTemplate {..}

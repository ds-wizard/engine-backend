module Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.Common.Database.Mapping.Common.SemVer2Tuple ()
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplatePhase ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateState ()
import Wizard.Model.DocumentTemplate.DocumentTemplateList

instance FromRow DocumentTemplateList where
  fromRow = do
    tId <- field
    name <- field
    organizationId <- field
    templateId <- field
    version <- field
    phase <- field
    metamodelVersion <- field
    description <- field
    allowedPackages <- fieldWith fromJSONField
    nonEditable <- field
    remoteVersion <- field
    remoteOrganizationName <- field
    remoteOrganizationLogo <- field
    createdAt <- field
    return $ DocumentTemplateList {..}

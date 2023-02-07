module Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplatePhase ()
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateState ()
import Wizard.Model.DocumentTemplate.DocumentTemplateList

instance FromRow DocumentTemplateList where
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
    state <- field
    remoteVersion <- field
    remoteOrganizationName <- field
    remoteOrganizationLogo <- field
    return $ DocumentTemplateList {..}

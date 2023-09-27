module Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateState ()
import Wizard.Model.DocumentTemplate.DocumentTemplateList
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplatePhase ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

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
    tenantUuid <- field
    updatedAt <- field
    phase <- field
    nonEditable <- field
    state <- field
    remoteVersion <- field
    remoteOrganizationName <- field
    remoteOrganizationLogo <- field
    return $ DocumentTemplateList {..}

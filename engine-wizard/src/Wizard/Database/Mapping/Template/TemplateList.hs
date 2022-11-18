module Wizard.Database.Mapping.Template.TemplateList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.Api.Resource.Template.TemplateJM ()
import Shared.Model.Template.TemplateJM ()
import Wizard.Database.Mapping.Template.TemplateState ()
import Wizard.Model.Template.TemplateList

instance FromRow TemplateList where
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
    state <- field
    remoteVersion <- field
    remoteOrganizationName <- field
    remoteOrganizationLogo <- field
    return $ TemplateList {..}

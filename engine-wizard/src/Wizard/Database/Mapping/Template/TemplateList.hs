module Wizard.Database.Mapping.Template.TemplateList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.Api.Resource.Template.TemplateJM ()
import Shared.Model.Template.TemplateJM ()
import Wizard.Model.Template.TemplateList

instance FromRow TemplateList where
  fromRow = do
    _templateListTId <- field
    _templateListName <- field
    _templateListOrganizationId <- field
    _templateListTemplateId <- field
    _templateListVersion <- field
    _templateListMetamodelVersion <- field
    _templateListDescription <- field
    _templateListReadme <- field
    _templateListLicense <- field
    _templateListAllowedPackages <- fieldWith fromJSONField
    _templateListRecommendedPackageId <- field
    _templateListFormats <- fieldWith fromJSONField
    _templateListCreatedAt <- field
    _templateListAppUuid <- field
    _templateListRemoteVersion <- field
    _templateListRemoteOrganizationName <- field
    _templateListRemoteOrganizationLogo <- field
    return $ TemplateList {..}

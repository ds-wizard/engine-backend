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
    [ toField _templateTId
    , toField _templateName
    , toField _templateOrganizationId
    , toField _templateTemplateId
    , toField _templateVersion
    , toField _templateMetamodelVersion
    , toField _templateDescription
    , toField _templateReadme
    , toField _templateLicense
    , toJSONField _templateAllowedPackages
    , toField _templateRecommendedPackageId
    , toJSONField _templateFormats
    , toField _templateCreatedAt
    , toField _templateAppUuid
    ]

instance FromRow Template where
  fromRow = do
    _templateTId <- field
    _templateName <- field
    _templateOrganizationId <- field
    _templateTemplateId <- field
    _templateVersion <- field
    _templateMetamodelVersion <- field
    _templateDescription <- field
    _templateReadme <- field
    _templateLicense <- field
    _templateAllowedPackages <- fieldWith fromJSONField
    _templateRecommendedPackageId <- field
    _templateFormats <- fieldWith fromJSONField
    _templateCreatedAt <- field
    _templateAppUuid <- field
    return $ Template {..}

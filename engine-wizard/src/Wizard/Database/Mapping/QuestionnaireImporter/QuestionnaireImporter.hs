module Wizard.Database.Mapping.QuestionnaireImporter.QuestionnaireImporter where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Api.Resource.Package.PackagePatternJM ()
import Wizard.Model.QuestionnaireImporter.QuestionnaireImporter

instance ToRow QuestionnaireImporter where
  toRow QuestionnaireImporter {..} =
    [ toField _questionnaireImporterQiId
    , toField _questionnaireImporterName
    , toField _questionnaireImporterOrganizationId
    , toField _questionnaireImporterImporterId
    , toField _questionnaireImporterVersion
    , toField _questionnaireImporterMetamodelVersion
    , toField _questionnaireImporterDescription
    , toField _questionnaireImporterReadme
    , toField _questionnaireImporterLicense
    , toJSONField _questionnaireImporterAllowedPackages
    , toField _questionnaireImporterUrl
    , toField _questionnaireImporterEnabled
    , toField _questionnaireImporterAppUuid
    , toField _questionnaireImporterCreatedAt
    , toField _questionnaireImporterUpdatedAt
    ]

instance FromRow QuestionnaireImporter where
  fromRow = do
    _questionnaireImporterQiId <- field
    _questionnaireImporterName <- field
    _questionnaireImporterOrganizationId <- field
    _questionnaireImporterImporterId <- field
    _questionnaireImporterVersion <- field
    _questionnaireImporterMetamodelVersion <- field
    _questionnaireImporterDescription <- field
    _questionnaireImporterReadme <- field
    _questionnaireImporterLicense <- field
    _questionnaireImporterAllowedPackages <- fieldWith fromJSONField
    _questionnaireImporterUrl <- field
    _questionnaireImporterEnabled <- field
    _questionnaireImporterAppUuid <- field
    _questionnaireImporterCreatedAt <- field
    _questionnaireImporterUpdatedAt <- field
    return $ QuestionnaireImporter {..}

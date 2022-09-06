module Wizard.Database.Migration.Development.QuestionnaireImporter.Data.QuestionnaireImporters where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Constant.App
import Shared.Constant.QuestionnaireImporter
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Model.QuestionnaireImporter.QuestionnaireImporter
import Wizard.Service.QuestionnaireImporter.QuestionnaireImporterMapper

questionnaireImporterBio1 :: QuestionnaireImporter
questionnaireImporterBio1 =
  QuestionnaireImporter
    { _questionnaireImporterQiId = "global:questionnaire-importer-bio:1.0.0"
    , _questionnaireImporterName = "QuestionnaireImporterBio"
    , _questionnaireImporterOrganizationId = "global"
    , _questionnaireImporterImporterId = "questionnaire-importer-bio"
    , _questionnaireImporterVersion = "1.0.0"
    , _questionnaireImporterMetamodelVersion = questionnaireImporterMetamodelVersion
    , _questionnaireImporterDescription = "Import bio answers from questionnaire"
    , _questionnaireImporterReadme = "# Default QuestionnaireImporter BIO 1"
    , _questionnaireImporterLicense = "Apache-2.0"
    , _questionnaireImporterAllowedPackages = [packagePatternAll]
    , _questionnaireImporterUrl = "http://example.com/questionnaire-importer-bio"
    , _questionnaireImporterEnabled = True
    , _questionnaireImporterAppUuid = defaultAppUuid
    , _questionnaireImporterCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , _questionnaireImporterUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

questionnaireImporterBio2 :: QuestionnaireImporter
questionnaireImporterBio2 =
  QuestionnaireImporter
    { _questionnaireImporterQiId = "global:questionnaire-importer-bio:2.0.0"
    , _questionnaireImporterName = "QuestionnaireImporterBio"
    , _questionnaireImporterOrganizationId = "global"
    , _questionnaireImporterImporterId = "questionnaire-importer-bio"
    , _questionnaireImporterVersion = "2.0.0"
    , _questionnaireImporterMetamodelVersion = questionnaireImporterMetamodelVersion
    , _questionnaireImporterDescription = "Import bio answers from questionnaire"
    , _questionnaireImporterReadme = "# Default QuestionnaireImporter BIO 2"
    , _questionnaireImporterLicense = "Apache-2.0"
    , _questionnaireImporterAllowedPackages = [packagePatternAll]
    , _questionnaireImporterUrl = "http://example.com/questionnaire-importer-bio"
    , _questionnaireImporterEnabled = True
    , _questionnaireImporterAppUuid = defaultAppUuid
    , _questionnaireImporterCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , _questionnaireImporterUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

questionnaireImporterBio3 :: QuestionnaireImporter
questionnaireImporterBio3 =
  QuestionnaireImporter
    { _questionnaireImporterQiId = "global:questionnaire-importer-bio:3.0.0"
    , _questionnaireImporterName = "QuestionnaireImporterBio"
    , _questionnaireImporterOrganizationId = "global"
    , _questionnaireImporterImporterId = "questionnaire-importer-bio"
    , _questionnaireImporterVersion = "3.0.0"
    , _questionnaireImporterMetamodelVersion = questionnaireImporterMetamodelVersion
    , _questionnaireImporterDescription = "Import bio answers from questionnaire"
    , _questionnaireImporterReadme = "# Default QuestionnaireImporter BIO 3"
    , _questionnaireImporterLicense = "Apache-2.0"
    , _questionnaireImporterAllowedPackages = [packagePatternAll]
    , _questionnaireImporterUrl = "http://example.com/questionnaire-importer-bio"
    , _questionnaireImporterEnabled = False
    , _questionnaireImporterAppUuid = defaultAppUuid
    , _questionnaireImporterCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , _questionnaireImporterUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

questionnaireImporterBio3Edited :: QuestionnaireImporter
questionnaireImporterBio3Edited = questionnaireImporterBio3 {_questionnaireImporterEnabled = True}

questionnaireImporterBio3Dto :: QuestionnaireImporterDTO
questionnaireImporterBio3Dto = toDTO questionnaireImporterBio3

questionnaireExtImporter1 :: QuestionnaireImporter
questionnaireExtImporter1 =
  QuestionnaireImporter
    { _questionnaireImporterQiId = "global:questionnaire-ext-importer:1.0.0"
    , _questionnaireImporterName = "QuestionnaireExtImporter"
    , _questionnaireImporterOrganizationId = "global"
    , _questionnaireImporterImporterId = "questionnaire-ext-importer"
    , _questionnaireImporterVersion = "1.0.0"
    , _questionnaireImporterMetamodelVersion = questionnaireImporterMetamodelVersion
    , _questionnaireImporterDescription = "Import ext answers from questionnaire"
    , _questionnaireImporterReadme = "# Default Ext QuestionnaireImporter"
    , _questionnaireImporterLicense = "Apache-2.0"
    , _questionnaireImporterAllowedPackages = [packagePatternGlobal]
    , _questionnaireImporterUrl = "http://example.com/questionnaire-ext-importer"
    , _questionnaireImporterEnabled = True
    , _questionnaireImporterAppUuid = defaultAppUuid
    , _questionnaireImporterCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , _questionnaireImporterUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

questionnaireOntoImporter1 :: QuestionnaireImporter
questionnaireOntoImporter1 =
  QuestionnaireImporter
    { _questionnaireImporterQiId = "global:questionnaire-onto-importer:1.0.0"
    , _questionnaireImporterName = "QuestionnaireOntoImporter"
    , _questionnaireImporterOrganizationId = "global"
    , _questionnaireImporterImporterId = "questionnaire-onto-importer"
    , _questionnaireImporterVersion = "1.0.0"
    , _questionnaireImporterMetamodelVersion = questionnaireImporterMetamodelVersion
    , _questionnaireImporterDescription = "Import onto answers from questionnaire"
    , _questionnaireImporterReadme = "# Default Ext QuestionnaireImporter"
    , _questionnaireImporterLicense = "Apache-2.0"
    , _questionnaireImporterAllowedPackages = [packagePatternGlobal]
    , _questionnaireImporterUrl = "http://example.com/questionnaire-onto-importer"
    , _questionnaireImporterEnabled = False
    , _questionnaireImporterAppUuid = defaultAppUuid
    , _questionnaireImporterCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , _questionnaireImporterUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

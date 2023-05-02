module Wizard.Database.Migration.Development.QuestionnaireImporter.Data.QuestionnaireImporters where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Constant.App
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Model.QuestionnaireImporter.QuestionnaireImporter
import Wizard.Service.QuestionnaireImporter.QuestionnaireImporterMapper
import WizardLib.Common.Constant.QuestionnaireImporter
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

questionnaireImporterBio1 :: QuestionnaireImporter
questionnaireImporterBio1 =
  QuestionnaireImporter
    { qiId = "global:questionnaire-importer-bio:1.0.0"
    , name = "QuestionnaireImporterBio"
    , organizationId = "global"
    , importerId = "questionnaire-importer-bio"
    , version = "1.0.0"
    , metamodelVersion = questionnaireImporterMetamodelVersion
    , description = "Import bio answers from questionnaire"
    , readme = "# Default QuestionnaireImporter BIO 1"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternAll]
    , url = "http://example.com/questionnaire-importer-bio"
    , enabled = True
    , appUuid = defaultAppUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

questionnaireImporterBio2 :: QuestionnaireImporter
questionnaireImporterBio2 =
  QuestionnaireImporter
    { qiId = "global:questionnaire-importer-bio:2.0.0"
    , name = "QuestionnaireImporterBio"
    , organizationId = "global"
    , importerId = "questionnaire-importer-bio"
    , version = "2.0.0"
    , metamodelVersion = questionnaireImporterMetamodelVersion
    , description = "Import bio answers from questionnaire"
    , readme = "# Default QuestionnaireImporter BIO 2"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternAll]
    , url = "http://example.com/questionnaire-importer-bio"
    , enabled = True
    , appUuid = defaultAppUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

questionnaireImporterBio3 :: QuestionnaireImporter
questionnaireImporterBio3 =
  QuestionnaireImporter
    { qiId = "global:questionnaire-importer-bio:3.0.0"
    , name = "QuestionnaireImporterBio"
    , organizationId = "global"
    , importerId = "questionnaire-importer-bio"
    , version = "3.0.0"
    , metamodelVersion = questionnaireImporterMetamodelVersion
    , description = "Import bio answers from questionnaire"
    , readme = "# Default QuestionnaireImporter BIO 3"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternAll]
    , url = "http://example.com/questionnaire-importer-bio"
    , enabled = False
    , appUuid = defaultAppUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

questionnaireImporterBio3Edited :: QuestionnaireImporter
questionnaireImporterBio3Edited = questionnaireImporterBio3 {enabled = True}

questionnaireImporterBio3Dto :: QuestionnaireImporterDTO
questionnaireImporterBio3Dto = toDTO questionnaireImporterBio3

questionnaireExtImporter1 :: QuestionnaireImporter
questionnaireExtImporter1 =
  QuestionnaireImporter
    { qiId = "global:questionnaire-ext-importer:1.0.0"
    , name = "QuestionnaireExtImporter"
    , organizationId = "global"
    , importerId = "questionnaire-ext-importer"
    , version = "1.0.0"
    , metamodelVersion = questionnaireImporterMetamodelVersion
    , description = "Import ext answers from questionnaire"
    , readme = "# Default Ext QuestionnaireImporter"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternGlobal]
    , url = "http://example.com/questionnaire-ext-importer"
    , enabled = True
    , appUuid = defaultAppUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

questionnaireOntoImporter1 :: QuestionnaireImporter
questionnaireOntoImporter1 =
  QuestionnaireImporter
    { qiId = "global:questionnaire-onto-importer:1.0.0"
    , name = "QuestionnaireOntoImporter"
    , organizationId = "global"
    , importerId = "questionnaire-onto-importer"
    , version = "1.0.0"
    , metamodelVersion = questionnaireImporterMetamodelVersion
    , description = "Import onto answers from questionnaire"
    , readme = "# Default Ext QuestionnaireImporter"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternGlobal]
    , url = "http://example.com/questionnaire-onto-importer"
    , enabled = False
    , appUuid = defaultAppUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

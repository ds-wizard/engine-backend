module Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Constant.App
import Shared.Constant.DocumentTemplate
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.DocumentTemplate.DocumentTemplate

wizardDocumentTemplate :: DocumentTemplate
wizardDocumentTemplate =
  DocumentTemplate
    { tId = "global:questionnaire-report:1.0.0"
    , name = "Questionnaire Report"
    , organizationId = "global"
    , templateId = "questionnaire-report"
    , version = "1.0.0"
    , phase = ReleasedDocumentTemplatePhase
    , metamodelVersion = documentTemplateMetamodelVersion
    , description = "Exported questions and answers from a questionnaire"
    , readme = "# Default DocumentTemplate"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternAll]
    , formats =
        [ formatJson
        , formatHtml
        , formatPdf
        , formatLatex
        , formatDocx
        , formatOdt
        , formatMarkdown
        ]
    , appUuid = defaultAppUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

wizardDocumentTemplateDeprecated :: DocumentTemplate
wizardDocumentTemplateDeprecated = wizardDocumentTemplate {phase = DeprecatedDocumentTemplatePhase}

wizardDocumentTemplateDraft :: DocumentTemplate
wizardDocumentTemplateDraft =
  wizardDocumentTemplate
    { tId = "global:questionnaire-report:2.0.0"
    , name = "DRAFT: " ++ wizardDocumentTemplate.name
    , version = "2.0.0"
    , phase = DraftDocumentTemplatePhase
    , description = "DRAFT: " ++ wizardDocumentTemplate.description
    , allowedPackages = [packagePatternAllEdited]
    }

wizardDocumentTemplateNlDraft :: DocumentTemplate
wizardDocumentTemplateNlDraft =
  wizardDocumentTemplate
    { tId = "org.nl.amsterdam:questionnaire-report:3.0.0"
    , name = "New Document Template"
    , organizationId = "org.nl.amsterdam"
    , version = "3.0.0"
    , phase = DraftDocumentTemplatePhase
    , description = ""
    , allowedPackages = []
    }

anotherWizardDocumentTemplate :: DocumentTemplate
anotherWizardDocumentTemplate =
  DocumentTemplate
    { tId = "dsw:another-temlate:1.0.0"
    , name = "Another DocumentTemplate"
    , organizationId = "dsw"
    , templateId = "another-template"
    , version = "1.0.0"
    , phase = ReleasedDocumentTemplatePhase
    , metamodelVersion = documentTemplateMetamodelVersion
    , description = "This is a another template"
    , readme = "# Another DocumentTemplate"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternAll]
    , formats =
        [ formatJson
        , formatHtml
        , formatPdf
        , formatLatex
        , formatDocx
        , formatOdt
        , formatMarkdown
        ]
    , appUuid = defaultAppUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

-- ---------------------------------------------------------------------------------------------------------------------
differentDocumentTemplate :: DocumentTemplate
differentDocumentTemplate =
  DocumentTemplate
    { tId = "dsw:another-temlate:1.0.0"
    , name = "Another DocumentTemplate"
    , organizationId = "dsw"
    , templateId = "another-template"
    , version = "1.0.0"
    , phase = ReleasedDocumentTemplatePhase
    , metamodelVersion = documentTemplateMetamodelVersion
    , description = "This is a another template"
    , readme = "# Another DocumentTemplate"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternAll]
    , formats = []
    , appUuid = differentAppUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

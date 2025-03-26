module Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs where

import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Model.Common.SensitiveData
import Shared.Common.Model.Config.SimpleFeature
import Shared.OpenId.Database.Migration.Development.OpenId.Data.OpenIds
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Config.TenantConfigEM ()
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantConfigs

defaultSecret = "01234567890123456789012345678901"

defaultTenantConfig :: TenantConfig
defaultTenantConfig =
  TenantConfig
    { uuid = defaultTenant.uuid
    , organization = defaultOrganization
    , authentication = defaultAuth
    , privacyAndSupport = defaultPrivacyAndSupport
    , dashboardAndLoginScreen = defaultDashboardAndLoginScreen
    , lookAndFeel = defaultLookAndFeel
    , registry = defaultRegistry
    , knowledgeModel = defaultKnowledgeModel
    , questionnaire = defaultQuestionnaire
    , submission = defaultSubmission
    , owl = defaultOwl
    , mailConfigUuid = Nothing
    , aiAssistant = defaultAiAssistant
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

defaultTenantConfigEncrypted :: TenantConfig
defaultTenantConfigEncrypted = process defaultSecret defaultTenantConfig

defaultAuth :: TenantConfigAuth
defaultAuth =
  TenantConfigAuth
    { defaultRole = _USER_ROLE_RESEARCHER
    , internal = defaultAuthInternal
    , external = defaultAuthExternal
    }

defaultAuthInternal :: TenantConfigAuthInternal
defaultAuthInternal = TenantConfigAuthInternal {registration = SimpleFeature True, twoFactorAuth = defaultAuthInternalTwoFactorAuth}

defaultAuthInternalTwoFactorAuth :: TenantConfigAuthInternalTwoFactorAuth
defaultAuthInternalTwoFactorAuth =
  TenantConfigAuthInternalTwoFactorAuth
    { enabled = False
    , codeLength = 6
    , expiration = 600
    }

defaultAuthExternal :: TenantConfigAuthExternal
defaultAuthExternal = TenantConfigAuthExternal {services = [defaultAuthExternalService]}

defaultAuthExternalService :: TenantConfigAuthExternalService
defaultAuthExternalService =
  TenantConfigAuthExternalService
    { aId = "google"
    , name = "Google"
    , url = "https://accounts.google.com"
    , clientId = "32559869123-a98908094.apps.googleusercontent.com"
    , clientSecret = "sad89089023"
    , parameteres = [openIdClientDefinitionParameter]
    , style = Just openIdClientDefinitionStyle
    }

defaultPrivacyAndSupport :: TenantConfigPrivacyAndSupport
defaultPrivacyAndSupport =
  TenantConfigPrivacyAndSupport
    { privacyUrl = Nothing
    , termsOfServiceUrl = Nothing
    , supportEmail = Nothing
    , supportSiteName = Nothing
    , supportSiteUrl = Nothing
    , supportSiteIcon = Nothing
    }

defaultDashboardAndLoginScreen :: TenantConfigDashboardAndLoginScreen
defaultDashboardAndLoginScreen =
  TenantConfigDashboardAndLoginScreen
    { dashboardType = WelcomeDashboardType
    , announcements = [defaultDashboardAndLoginScreenAnnouncement]
    , loginInfo = Nothing
    , loginInfoSidebar = Nothing
    }

defaultLookAndFeel :: TenantConfigLookAndFeel
defaultLookAndFeel =
  TenantConfigLookAndFeel
    { appTitle = Nothing
    , appTitleShort = Nothing
    , customMenuLinks = [defaultLookAndFeelCustomLink]
    , logoUrl = Nothing
    , primaryColor = Nothing
    , illustrationsColor = Nothing
    }

defaultLookAndFeelCustomLink :: TenantConfigLookAndFeelCustomMenuLink
defaultLookAndFeelCustomLink =
  TenantConfigLookAndFeelCustomMenuLink
    { icon = "faq"
    , title = "My Link"
    , url = "http://example.prg"
    , newWindow = False
    }

defaultRegistry :: TenantConfigRegistry
defaultRegistry = TenantConfigRegistry {enabled = True, token = "GlobalToken"}

defaultKnowledgeModel :: TenantConfigKnowledgeModel
defaultKnowledgeModel =
  TenantConfigKnowledgeModel
    { public = defaultKnowledgeModelPublic
    , integrationConfig =
        "ontologyPortal: \n\
        \  path: ontology-portal.json \n\
        \bioPortal: \n\
        \  path: bio-portal.json "
    }

defaultKnowledgeModelPublic :: TenantConfigKnowledgeModelPublic
defaultKnowledgeModelPublic =
  TenantConfigKnowledgeModelPublic
    { enabled = True
    , packages = [packagePatternGlobal]
    }

defaultQuestionnaire :: TenantConfigQuestionnaire
defaultQuestionnaire =
  TenantConfigQuestionnaire
    { questionnaireVisibility = defaultQuestionnaireVisibility
    , questionnaireSharing = defaultQuestionnaireSharing
    , questionnaireCreation = TemplateAndCustomQuestionnaireCreation
    , projectTagging = defaultQuestionnaireProjectTagging
    , summaryReport = SimpleFeature True
    , feedback = defaultFeedback
    }

defaultQuestionnaireVisibility :: TenantConfigQuestionnaireVisibility
defaultQuestionnaireVisibility =
  TenantConfigQuestionnaireVisibility
    { enabled = True
    , defaultValue = PrivateQuestionnaire
    }

defaultQuestionnaireSharing :: TenantConfigQuestionnaireSharing
defaultQuestionnaireSharing =
  TenantConfigQuestionnaireSharing
    { enabled = True
    , defaultValue = RestrictedQuestionnaire
    , anonymousEnabled = False
    }

_SETTINGS_PROJECT_TAG_1 = "settingsProjectTag1"

_SETTINGS_PROJECT_TAG_2 = "settingsProjectTag2"

defaultQuestionnaireProjectTagging :: TenantConfigQuestionnaireProjectTagging
defaultQuestionnaireProjectTagging =
  TenantConfigQuestionnaireProjectTagging
    { enabled = True
    , tags = [_SETTINGS_PROJECT_TAG_1, _SETTINGS_PROJECT_TAG_2]
    }

defaultFeedback :: TenantConfigQuestionnaireFeedback
defaultFeedback =
  TenantConfigQuestionnaireFeedback
    { enabled = True
    , token = ""
    , owner = "DSWGlobal"
    , repo = "dsw-test"
    }

defaultSubmission :: TenantConfigSubmission
defaultSubmission =
  TenantConfigSubmission {enabled = True, services = [defaultSubmissionService]}

defaultSubmissionService :: TenantConfigSubmissionService
defaultSubmissionService =
  TenantConfigSubmissionService
    { sId = "mySubmissionServer"
    , name = "My Submission Server"
    , description = "Some description"
    , props = [defaultSubmissionServiceApiTokenProp, defaultSubmissionServiceSecretProp]
    , supportedFormats = [defaultSubmissionServiceSupportedFormat]
    , request = defaultSubmissionServiceRequest
    }

defaultSubmissionServiceApiTokenProp :: String
defaultSubmissionServiceApiTokenProp = "API Token"

defaultSubmissionServiceSecretProp :: String
defaultSubmissionServiceSecretProp = "Secret"

defaultSubmissionServiceSupportedFormat :: TenantConfigSubmissionServiceSupportedFormat
defaultSubmissionServiceSupportedFormat =
  TenantConfigSubmissionServiceSupportedFormat
    { templateId = wizardDocumentTemplate.tId
    , formatUuid = formatJson.uuid
    }

defaultSubmissionServiceRequest :: TenantConfigSubmissionServiceRequest
defaultSubmissionServiceRequest =
  TenantConfigSubmissionServiceRequest
    { method = "GET"
    , url = "https://mockserver.ds-wizard.org/submission.json"
    , headers = M.fromList [("Api-Key", "${API Token}")]
    , multipart = defaultSubmissionServiceRequestMultipart
    }

defaultSubmissionServiceRequestMultipart :: TenantConfigSubmissionServiceRequestMultipart
defaultSubmissionServiceRequestMultipart =
  TenantConfigSubmissionServiceRequestMultipart
    { enabled = False
    , fileName = "file"
    }

defaultOwl :: TenantConfigOwl
defaultOwl =
  TenantConfigOwl
    { enabled = False
    , name = ""
    , organizationId = ""
    , kmId = ""
    , version = ""
    , previousPackageId = Nothing
    , rootElement = ""
    }

defaultAiAssistant :: TenantConfigAiAssistant
defaultAiAssistant =
  TenantConfigAiAssistant
    { enabled = True
    }

differentTenantConfig :: TenantConfig
differentTenantConfig = defaultTenantConfig {uuid = differentTenant.uuid}

differentTenantConfigEncrypted :: TenantConfig
differentTenantConfigEncrypted = process defaultSecret differentTenantConfig

-- ------------------------------------------------------------
-- ------------------------------------------------------------
editedTenantConfig :: TenantConfig
editedTenantConfig = defaultTenantConfig {questionnaire = editedQuestionnaire}

editedQuestionnaire :: TenantConfigQuestionnaire
editedQuestionnaire = defaultQuestionnaire {summaryReport = SimpleFeature False}

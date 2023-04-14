module Wizard.Database.Migration.Development.Config.Data.AppConfigs where

import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.DocumentTemplate.DocumentTemplate
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.App.App
import Wizard.Model.Common.SensitiveData
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.AppConfigEM ()
import Wizard.Model.Config.SimpleFeature
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.User.User

defaultSecret = "01234567890123456789012345678901"

defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig
    { uuid = defaultApp.uuid
    , organization = defaultOrganization
    , feature = defaultFeature
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
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

defaultAppConfigEncrypted :: AppConfig
defaultAppConfigEncrypted = process defaultSecret defaultAppConfig

defaultOrganization :: AppConfigOrganization
defaultOrganization =
  AppConfigOrganization
    { name = "Organization Amsterdam"
    , description = "Some description of Organization Amsterdam"
    , organizationId = "org.nl.amsterdam"
    , affiliations = []
    }

defaultFeature :: AppConfigFeature
defaultFeature =
  AppConfigFeature
    { clientCustomizationEnabled = True
    , pdfOnlyEnabled = False
    , pdfWatermarkEnabled = False
    }

defaultAuth :: AppConfigAuth
defaultAuth =
  AppConfigAuth
    { defaultRole = _USER_ROLE_DATA_STEWARD
    , internal = defaultAuthInternal
    , external = defaultAuthExternal
    }

defaultAuthInternal :: AppConfigAuthInternal
defaultAuthInternal = AppConfigAuthInternal {registration = SimpleFeature True, twoFactorAuth = defaultAuthInternalTwoFactorAuth}

defaultAuthInternalTwoFactorAuth :: AppConfigAuthInternalTwoFactorAuth
defaultAuthInternalTwoFactorAuth =
  AppConfigAuthInternalTwoFactorAuth
    { enabled = False
    , codeLength = 6
    , expiration = 600
    }

defaultAuthExternal :: AppConfigAuthExternal
defaultAuthExternal = AppConfigAuthExternal {services = [defaultAuthExternalService]}

defaultAuthExternalService :: AppConfigAuthExternalService
defaultAuthExternalService =
  AppConfigAuthExternalService
    { aId = "google"
    , name = "Google"
    , url = "https://accounts.google.com"
    , clientId = "32559869123-a98908094.apps.googleusercontent.com"
    , clientSecret = "sad89089023"
    , parameteres = [defaultAuthExternalServiceParameter]
    , style = Just defaultAuthExternalServiceStyle
    }

defaultAuthExternalServiceParameter :: AppConfigAuthExternalServiceParameter
defaultAuthExternalServiceParameter =
  AppConfigAuthExternalServiceParameter
    { name = "hd2"
    , value = "google.com"
    }

defaultAuthExternalServiceStyle :: AppConfigAuthExternalServiceStyle
defaultAuthExternalServiceStyle =
  AppConfigAuthExternalServiceStyle
    { icon = Just "fa-google"
    , background = Just "#000"
    , color = Just "#FFF"
    }

defaultPrivacyAndSupport :: AppConfigPrivacyAndSupport
defaultPrivacyAndSupport =
  AppConfigPrivacyAndSupport
    { privacyUrl = Nothing
    , termsOfServiceUrl = Nothing
    , supportEmail = Nothing
    , supportSiteName = Nothing
    , supportSiteUrl = Nothing
    , supportSiteIcon = Nothing
    }

defaultDashboardAndLoginScreen :: AppConfigDashboardAndLoginScreen
defaultDashboardAndLoginScreen =
  AppConfigDashboardAndLoginScreen
    { dashboardType = WelcomeDashboardType
    , announcements = [defaultDashboardAndLoginScreenAnnouncement]
    , loginInfo = Nothing
    }

defaultDashboardAndLoginScreenAnnouncement :: AppConfigDashboardAndLoginScreenAnnouncement
defaultDashboardAndLoginScreenAnnouncement =
  AppConfigDashboardAndLoginScreenAnnouncement
    { content = "Hello"
    , level = InfoAnnouncementLevelType
    , dashboard = True
    , loginScreen = True
    }

defaultLookAndFeel :: AppConfigLookAndFeel
defaultLookAndFeel =
  AppConfigLookAndFeel
    { appTitle = Nothing
    , appTitleShort = Nothing
    , customMenuLinks = [defaultLookAndFeelCustomLink]
    , logoUrl = Nothing
    , styleUrl = Nothing
    , primaryColor = Nothing
    , illustrationsColor = Nothing
    }

defaultLookAndFeelCustomLink :: AppConfigLookAndFeelCustomMenuLink
defaultLookAndFeelCustomLink =
  AppConfigLookAndFeelCustomMenuLink
    { icon = "faq"
    , title = "My Link"
    , url = "http://example.prg"
    , newWindow = False
    }

defaultRegistry :: AppConfigRegistry
defaultRegistry = AppConfigRegistry {enabled = True, token = "GlobalToken"}

defaultKnowledgeModel :: AppConfigKnowledgeModel
defaultKnowledgeModel =
  AppConfigKnowledgeModel
    { public = defaultKnowledgeModelPublic
    , integrationConfig =
        "ontologyPortal: \n\
        \  path: ontology-portal.json \n\
        \bioPortal: \n\
        \  path: bio-portal.json "
    }

defaultKnowledgeModelPublic :: AppConfigKnowledgeModelPublic
defaultKnowledgeModelPublic =
  AppConfigKnowledgeModelPublic
    { enabled = True
    , packages = [packagePatternGlobal]
    }

defaultQuestionnaire :: AppConfigQuestionnaire
defaultQuestionnaire =
  AppConfigQuestionnaire
    { questionnaireVisibility = defaultQuestionnaireVisibility
    , questionnaireSharing = defaultQuestionnaireSharing
    , questionnaireCreation = TemplateAndCustomQuestionnaireCreation
    , projectTagging = defaultQuestionnaireProjectTagging
    , summaryReport = SimpleFeature True
    , feedback = defaultFeedback
    }

defaultQuestionnaireVisibility :: AppConfigQuestionnaireVisibility
defaultQuestionnaireVisibility =
  AppConfigQuestionnaireVisibility
    { enabled = True
    , defaultValue = PrivateQuestionnaire
    }

defaultQuestionnaireSharing :: AppConfigQuestionnaireSharing
defaultQuestionnaireSharing =
  AppConfigQuestionnaireSharing
    { enabled = True
    , defaultValue = RestrictedQuestionnaire
    , anonymousEnabled = False
    }

_SETTINGS_PROJECT_TAG_1 = "settingsProjectTag1"

_SETTINGS_PROJECT_TAG_2 = "settingsProjectTag2"

defaultQuestionnaireProjectTagging :: AppConfigQuestionnaireProjectTagging
defaultQuestionnaireProjectTagging =
  AppConfigQuestionnaireProjectTagging
    { enabled = True
    , tags = [_SETTINGS_PROJECT_TAG_1, _SETTINGS_PROJECT_TAG_2]
    }

defaultFeedback :: AppConfigQuestionnaireFeedback
defaultFeedback =
  AppConfigQuestionnaireFeedback
    { enabled = True
    , token = ""
    , owner = "DSWGlobal"
    , repo = "dsw-test"
    }

defaultSubmission :: AppConfigSubmission
defaultSubmission =
  AppConfigSubmission {enabled = True, services = [defaultSubmissionService]}

defaultSubmissionService :: AppConfigSubmissionService
defaultSubmissionService =
  AppConfigSubmissionService
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

defaultSubmissionServiceSupportedFormat :: AppConfigSubmissionServiceSupportedFormat
defaultSubmissionServiceSupportedFormat =
  AppConfigSubmissionServiceSupportedFormat
    { templateId = wizardDocumentTemplate.tId
    , formatUuid = formatJson.uuid
    }

defaultSubmissionServiceRequest :: AppConfigSubmissionServiceRequest
defaultSubmissionServiceRequest =
  AppConfigSubmissionServiceRequest
    { method = "GET"
    , url = "https://mockserver.ds-wizard.org/submission.json"
    , headers = M.fromList [("Api-Key", "${API Token}")]
    , multipart = defaultSubmissionServiceRequestMultipart
    }

defaultSubmissionServiceRequestMultipart :: AppConfigSubmissionServiceRequestMultipart
defaultSubmissionServiceRequestMultipart =
  AppConfigSubmissionServiceRequestMultipart
    { enabled = False
    , fileName = "file"
    }

defaultOwl :: AppConfigOwl
defaultOwl =
  AppConfigOwl
    { enabled = False
    , name = ""
    , organizationId = ""
    , kmId = ""
    , version = ""
    , previousPackageId = Nothing
    , rootElement = ""
    }

differentAppConfig :: AppConfig
differentAppConfig = defaultAppConfig {uuid = differentApp.uuid}

differentAppConfigEncrypted :: AppConfig
differentAppConfigEncrypted = process defaultSecret differentAppConfig

-- ------------------------------------------------------------
-- ------------------------------------------------------------
editedAppConfig :: AppConfig
editedAppConfig = defaultAppConfig {questionnaire = editedQuestionnaire}

editedQuestionnaire :: AppConfigQuestionnaire
editedQuestionnaire = defaultQuestionnaire {summaryReport = SimpleFeature False}

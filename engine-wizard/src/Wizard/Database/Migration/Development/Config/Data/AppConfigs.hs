module Wizard.Database.Migration.Development.Config.Data.AppConfigs where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.Template.Data.Templates
import Wizard.Database.Migration.Development.App.Data.Apps
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
    { _appConfigUuid = defaultApp ^. uuid
    , _appConfigOrganization = defaultOrganization
    , _appConfigFeature = defaultFeature
    , _appConfigAuthentication = defaultAuth
    , _appConfigPrivacyAndSupport = defaultPrivacyAndSupport
    , _appConfigDashboard = defaultDashboard
    , _appConfigLookAndFeel = defaultLookAndFeel
    , _appConfigRegistry = defaultRegistry
    , _appConfigKnowledgeModel = defaultKnowledgeModel
    , _appConfigQuestionnaire = defaultQuestionnaire
    , _appConfigTemplate = defaultTemplate
    , _appConfigSubmission = defaultSubmission
    , _appConfigOwl = defaultOwl
    , _appConfigCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _appConfigUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

defaultAppConfigEncrypted :: AppConfig
defaultAppConfigEncrypted = process defaultSecret defaultAppConfig

defaultOrganization :: AppConfigOrganization
defaultOrganization =
  AppConfigOrganization
    { _appConfigOrganizationName = "Organization Amsterdam"
    , _appConfigOrganizationDescription = "Some description of Organization Amsterdam"
    , _appConfigOrganizationOrganizationId = "org.nl.amsterdam"
    , _appConfigOrganizationAffiliations = []
    }

defaultFeature :: AppConfigFeature
defaultFeature =
  AppConfigFeature
    { _appConfigFeatureClientCustomizationEnabled = True
    , _appConfigFeaturePdfOnlyEnabled = False
    , _appConfigFeaturePdfWatermarkEnabled = False
    }

defaultAuth :: AppConfigAuth
defaultAuth =
  AppConfigAuth
    { _appConfigAuthDefaultRole = _USER_ROLE_DATA_STEWARD
    , _appConfigAuthInternal = defaultAuthInternal
    , _appConfigAuthExternal = defaultAuthExternal
    }

defaultAuthInternal :: AppConfigAuthInternal
defaultAuthInternal = AppConfigAuthInternal {_appConfigAuthInternalRegistration = SimpleFeature True}

defaultAuthExternal :: AppConfigAuthExternal
defaultAuthExternal = AppConfigAuthExternal {_appConfigAuthExternalServices = [defaultAuthExternalService]}

defaultAuthExternalService :: AppConfigAuthExternalService
defaultAuthExternalService =
  AppConfigAuthExternalService
    { _appConfigAuthExternalServiceAId = "google"
    , _appConfigAuthExternalServiceName = "Google"
    , _appConfigAuthExternalServiceUrl = "https://accounts.google.com"
    , _appConfigAuthExternalServiceClientId = "32559869123-a98908094.apps.googleusercontent.com"
    , _appConfigAuthExternalServiceClientSecret = "sad89089023"
    , _appConfigAuthExternalServiceParameteres = [defaultAuthExternalServiceParameter]
    , _appConfigAuthExternalServiceStyle = Just defaultAuthExternalServiceStyle
    }

defaultAuthExternalServiceParameter :: AppConfigAuthExternalServiceParameter
defaultAuthExternalServiceParameter =
  AppConfigAuthExternalServiceParameter
    {_appConfigAuthExternalServiceParameterName = "hd2", _appConfigAuthExternalServiceParameterValue = "google.com"}

defaultAuthExternalServiceStyle :: AppConfigAuthExternalServiceStyle
defaultAuthExternalServiceStyle =
  AppConfigAuthExternalServiceStyle
    { _appConfigAuthExternalServiceStyleIcon = Just "fa-google"
    , _appConfigAuthExternalServiceStyleBackground = Just "#000"
    , _appConfigAuthExternalServiceStyleColor = Just "#FFF"
    }

defaultPrivacyAndSupport :: AppConfigPrivacyAndSupport
defaultPrivacyAndSupport =
  AppConfigPrivacyAndSupport
    { _appConfigPrivacyAndSupportPrivacyUrl = Nothing
    , _appConfigPrivacyAndSupportTermsOfServiceUrl = Nothing
    , _appConfigPrivacyAndSupportSupportEmail = Nothing
    , _appConfigPrivacyAndSupportSupportRepositoryName = Nothing
    , _appConfigPrivacyAndSupportSupportRepositoryUrl = Nothing
    }

defaultDashboard :: AppConfigDashboard
defaultDashboard =
  AppConfigDashboard
    { _appConfigDashboardWidgets = Just defaultDashboardWidgets
    , _appConfigDashboardWelcomeWarning = Nothing
    , _appConfigDashboardWelcomeInfo = Nothing
    }

defaultDashboardWidgets :: AppConfigDashboardWidgets
defaultDashboardWidgets =
  AppConfigDashboardWidgets
    { _appConfigDashboardWidgetsAdmin = ["Welcome"]
    , _appConfigDashboardWidgetsDataSteward = ["Welcome"]
    , _appConfigDashboardWidgetsResearcher = ["Welcome"]
    }

defaultLookAndFeel :: AppConfigLookAndFeel
defaultLookAndFeel =
  AppConfigLookAndFeel
    { _appConfigLookAndFeelAppTitle = Nothing
    , _appConfigLookAndFeelAppTitleShort = Nothing
    , _appConfigLookAndFeelCustomMenuLinks = [defaultLookAndFeelCustomLink]
    , _appConfigLookAndFeelLoginInfo = Nothing
    , _appConfigLookAndFeelLogoUrl = Nothing
    , _appConfigLookAndFeelStyleUrl = Nothing
    , _appConfigLookAndFeelPrimaryColor = Nothing
    , _appConfigLookAndFeelIllustrationsColor = Nothing
    }

defaultLookAndFeelCustomLink :: AppConfigLookAndFeelCustomMenuLink
defaultLookAndFeelCustomLink =
  AppConfigLookAndFeelCustomMenuLink
    { _appConfigLookAndFeelCustomMenuLinkIcon = "faq"
    , _appConfigLookAndFeelCustomMenuLinkTitle = "My Link"
    , _appConfigLookAndFeelCustomMenuLinkUrl = "http://example.prg"
    , _appConfigLookAndFeelCustomMenuLinkNewWindow = False
    }

defaultRegistry :: AppConfigRegistry
defaultRegistry = AppConfigRegistry {_appConfigRegistryEnabled = True, _appConfigRegistryToken = "GlobalToken"}

defaultKnowledgeModel :: AppConfigKnowledgeModel
defaultKnowledgeModel =
  AppConfigKnowledgeModel
    { _appConfigKnowledgeModelPublic = defaultKnowledgeModelPublic
    , _appConfigKnowledgeModelIntegrationConfig =
        "ontologyPortal: \n\
        \  path: ontology-portal.json \n\
        \bioPortal: \n\
        \  path: bio-portal.json "
    }

defaultKnowledgeModelPublic :: AppConfigKnowledgeModelPublic
defaultKnowledgeModelPublic =
  AppConfigKnowledgeModelPublic
    {_appConfigKnowledgeModelPublicEnabled = True, _appConfigKnowledgeModelPublicPackages = [packagePatternGlobal]}

defaultQuestionnaire :: AppConfigQuestionnaire
defaultQuestionnaire =
  AppConfigQuestionnaire
    { _appConfigQuestionnaireQuestionnaireVisibility = defaultQuestionnaireVisibility
    , _appConfigQuestionnaireQuestionnaireSharing = defaultQuestionnaireSharing
    , _appConfigQuestionnaireQuestionnaireCreation = TemplateAndCustomQuestionnaireCreation
    , _appConfigQuestionnaireProjectTagging = defaultQuestionnaireProjectTagging
    , _appConfigQuestionnaireSummaryReport = SimpleFeature True
    , _appConfigQuestionnaireFeedback = defaultFeedback
    }

defaultQuestionnaireVisibility :: AppConfigQuestionnaireVisibility
defaultQuestionnaireVisibility =
  AppConfigQuestionnaireVisibility
    { _appConfigQuestionnaireVisibilityEnabled = True
    , _appConfigQuestionnaireVisibilityDefaultValue = PrivateQuestionnaire
    }

defaultQuestionnaireSharing :: AppConfigQuestionnaireSharing
defaultQuestionnaireSharing =
  AppConfigQuestionnaireSharing
    { _appConfigQuestionnaireSharingEnabled = True
    , _appConfigQuestionnaireSharingDefaultValue = RestrictedQuestionnaire
    , _appConfigQuestionnaireSharingAnonymousEnabled = False
    }

_SETTINGS_PROJECT_TAG_1 = "settingsProjectTag1"

_SETTINGS_PROJECT_TAG_2 = "settingsProjectTag2"

defaultQuestionnaireProjectTagging :: AppConfigQuestionnaireProjectTagging
defaultQuestionnaireProjectTagging =
  AppConfigQuestionnaireProjectTagging
    { _appConfigQuestionnaireProjectTaggingEnabled = True
    , _appConfigQuestionnaireProjectTaggingTags = [_SETTINGS_PROJECT_TAG_1, _SETTINGS_PROJECT_TAG_2]
    }

defaultFeedback :: AppConfigQuestionnaireFeedback
defaultFeedback =
  AppConfigQuestionnaireFeedback
    { _appConfigQuestionnaireFeedbackEnabled = True
    , _appConfigQuestionnaireFeedbackToken = ""
    , _appConfigQuestionnaireFeedbackOwner = "DSWGlobal"
    , _appConfigQuestionnaireFeedbackRepo = "dsw-test"
    }

defaultTemplate :: AppConfigTemplate
defaultTemplate = AppConfigTemplate {_appConfigTemplateRecommendedTemplateId = Just $ commonWizardTemplate ^. tId}

defaultSubmission :: AppConfigSubmission
defaultSubmission =
  AppConfigSubmission {_appConfigSubmissionEnabled = True, _appConfigSubmissionServices = [defaultSubmissionService]}

defaultSubmissionService :: AppConfigSubmissionService
defaultSubmissionService =
  AppConfigSubmissionService
    { _appConfigSubmissionServiceSId = "mySubmissionServer"
    , _appConfigSubmissionServiceName = "My Submission Server"
    , _appConfigSubmissionServiceDescription = "Some description"
    , _appConfigSubmissionServiceProps = [defaultSubmissionServiceApiTokenProp, defaultSubmissionServiceSecretProp]
    , _appConfigSubmissionServiceSupportedFormats = [defaultSubmissionServiceSupportedFormat]
    , _appConfigSubmissionServiceRequest = defaultSubmissionServiceRequest
    }

defaultSubmissionServiceApiTokenProp :: String
defaultSubmissionServiceApiTokenProp = "API Token"

defaultSubmissionServiceSecretProp :: String
defaultSubmissionServiceSecretProp = "Secret"

defaultSubmissionServiceSupportedFormat :: AppConfigSubmissionServiceSupportedFormat
defaultSubmissionServiceSupportedFormat =
  AppConfigSubmissionServiceSupportedFormat
    { _appConfigSubmissionServiceSupportedFormatTemplateId = commonWizardTemplate ^. tId
    , _appConfigSubmissionServiceSupportedFormatFormatUuid = templateFormatJson ^. uuid
    }

defaultSubmissionServiceRequest :: AppConfigSubmissionServiceRequest
defaultSubmissionServiceRequest =
  AppConfigSubmissionServiceRequest
    { _appConfigSubmissionServiceRequestMethod = "GET"
    , _appConfigSubmissionServiceRequestUrl = "https://mockserver.ds-wizard.org/submission.json"
    , _appConfigSubmissionServiceRequestHeaders = M.fromList [("Api-Key", "${API Token}")]
    , _appConfigSubmissionServiceRequestMultipart = defaultSubmissionServiceRequestMultipart
    }

defaultSubmissionServiceRequestMultipart :: AppConfigSubmissionServiceRequestMultipart
defaultSubmissionServiceRequestMultipart =
  AppConfigSubmissionServiceRequestMultipart
    { _appConfigSubmissionServiceRequestMultipartEnabled = False
    , _appConfigSubmissionServiceRequestMultipartFileName = "file"
    }

defaultOwl :: AppConfigOwl
defaultOwl =
  AppConfigOwl
    { _appConfigOwlEnabled = False
    , _appConfigOwlName = ""
    , _appConfigOwlOrganizationId = ""
    , _appConfigOwlKmId = ""
    , _appConfigOwlVersion = ""
    , _appConfigOwlPreviousPackageId = Nothing
    , _appConfigOwlRootElement = ""
    }

differentAppConfig :: AppConfig
differentAppConfig = defaultAppConfig {_appConfigUuid = differentApp ^. uuid}

differentAppConfigEncrypted :: AppConfig
differentAppConfigEncrypted = process defaultSecret differentAppConfig

-- ------------------------------------------------------------
-- ------------------------------------------------------------
editedAppConfig :: AppConfig
editedAppConfig = defaultAppConfig {_appConfigQuestionnaire = editedQuestionnaire}

editedQuestionnaire :: AppConfigQuestionnaire
editedQuestionnaire = defaultQuestionnaire {_appConfigQuestionnaireSummaryReport = SimpleFeature False}

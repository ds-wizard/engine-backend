module Wizard.Model.Config.AppConfigDM where

import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.SimpleFeature
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.User.User

defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig
    { _appConfigUuid = U.nil
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
    , _appConfigCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _appConfigUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

defaultOrganization :: AppConfigOrganization
defaultOrganization =
  AppConfigOrganization
    { _appConfigOrganizationName = "My Organization"
    , _appConfigOrganizationDescription = "My description"
    , _appConfigOrganizationOrganizationId = "organization"
    , _appConfigOrganizationAffiliations = []
    }

defaultFeature :: AppConfigFeature
defaultFeature = AppConfigFeature {_appConfigFeatureClientCustomizationEnabled = True}

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
defaultAuthExternal = AppConfigAuthExternal {_appConfigAuthExternalServices = []}

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
    { _appConfigDashboardWidgetsAdmin = ["DMPWorkflow", "LevelsQuestionnaire"]
    , _appConfigDashboardWidgetsDataSteward = ["DMPWorkflow", "LevelsQuestionnaire"]
    , _appConfigDashboardWidgetsResearcher = ["DMPWorkflow", "LevelsQuestionnaire"]
    }

defaultLookAndFeel :: AppConfigLookAndFeel
defaultLookAndFeel =
  AppConfigLookAndFeel
    { _appConfigLookAndFeelAppTitle = Nothing
    , _appConfigLookAndFeelAppTitleShort = Nothing
    , _appConfigLookAndFeelCustomMenuLinks = []
    , _appConfigLookAndFeelLoginInfo = Nothing
    , _appConfigLookAndFeelLogoUrl = Nothing
    , _appConfigLookAndFeelStyleUrl = Nothing
    , _appConfigLookAndFeelPrimaryColor = Nothing
    , _appConfigLookAndFeelIllustrationsColor = Nothing
    }

defaultRegistry :: AppConfigRegistry
defaultRegistry = AppConfigRegistry {_appConfigRegistryEnabled = False, _appConfigRegistryToken = ""}

defaultKnowledgeModel :: AppConfigKnowledgeModel
defaultKnowledgeModel = AppConfigKnowledgeModel {_appConfigKnowledgeModelPublic = defaultKnowledgeModelPublic}

defaultKnowledgeModelPublic :: AppConfigKnowledgeModelPublic
defaultKnowledgeModelPublic =
  AppConfigKnowledgeModelPublic
    {_appConfigKnowledgeModelPublicEnabled = False, _appConfigKnowledgeModelPublicPackages = []}

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
    , _appConfigQuestionnaireSharingAnonymousEnabled = True
    }

defaultQuestionnaireProjectTagging :: AppConfigQuestionnaireProjectTagging
defaultQuestionnaireProjectTagging =
  AppConfigQuestionnaireProjectTagging
    {_appConfigQuestionnaireProjectTaggingEnabled = True, _appConfigQuestionnaireProjectTaggingTags = []}

defaultFeedback :: AppConfigQuestionnaireFeedback
defaultFeedback =
  AppConfigQuestionnaireFeedback
    { _appConfigQuestionnaireFeedbackEnabled = False
    , _appConfigQuestionnaireFeedbackToken = ""
    , _appConfigQuestionnaireFeedbackOwner = ""
    , _appConfigQuestionnaireFeedbackRepo = ""
    }

defaultTemplate :: AppConfigTemplate
defaultTemplate = AppConfigTemplate {_appConfigTemplateRecommendedTemplateId = Nothing}

defaultSubmission :: AppConfigSubmission
defaultSubmission = AppConfigSubmission {_appConfigSubmissionEnabled = False, _appConfigSubmissionServices = []}

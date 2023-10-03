module Wizard.Model.Config.AppConfigDM where

import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Config.SimpleFeature
import Wizard.Model.Config.AppConfig
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.User.User

defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig
    { uuid = U.nil
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

defaultOrganization :: AppConfigOrganization
defaultOrganization =
  AppConfigOrganization
    { name = "My Organization"
    , description = "My description"
    , organizationId = "organization"
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
    { defaultRole = _USER_ROLE_RESEARCHER
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
defaultAuthExternal = AppConfigAuthExternal {services = []}

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
    { dashboardType = RoleBasedDashboardType
    , announcements = []
    , loginInfo = Nothing
    , loginInfoSidebar = Nothing
    }

defaultLookAndFeel :: AppConfigLookAndFeel
defaultLookAndFeel =
  AppConfigLookAndFeel
    { appTitle = Nothing
    , appTitleShort = Nothing
    , customMenuLinks = []
    , logoUrl = Nothing
    , styleUrl = Nothing
    , primaryColor = Nothing
    , illustrationsColor = Nothing
    }

defaultRegistry :: AppConfigRegistry
defaultRegistry = AppConfigRegistry {enabled = False, token = ""}

defaultKnowledgeModel :: AppConfigKnowledgeModel
defaultKnowledgeModel =
  AppConfigKnowledgeModel
    { public = defaultKnowledgeModelPublic
    , integrationConfig = ""
    }

defaultKnowledgeModelPublic :: AppConfigKnowledgeModelPublic
defaultKnowledgeModelPublic =
  AppConfigKnowledgeModelPublic
    { enabled = False
    , packages = []
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
    , anonymousEnabled = True
    }

defaultQuestionnaireProjectTagging :: AppConfigQuestionnaireProjectTagging
defaultQuestionnaireProjectTagging =
  AppConfigQuestionnaireProjectTagging
    { enabled = True
    , tags = []
    }

defaultFeedback :: AppConfigQuestionnaireFeedback
defaultFeedback =
  AppConfigQuestionnaireFeedback
    { enabled = False
    , token = ""
    , owner = ""
    , repo = ""
    }

defaultSubmission :: AppConfigSubmission
defaultSubmission = AppConfigSubmission {enabled = False, services = []}

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

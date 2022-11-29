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
    { uuid = U.nil
    , organization = defaultOrganization
    , feature = defaultFeature
    , authentication = defaultAuth
    , privacyAndSupport = defaultPrivacyAndSupport
    , dashboard = defaultDashboard
    , lookAndFeel = defaultLookAndFeel
    , registry = defaultRegistry
    , knowledgeModel = defaultKnowledgeModel
    , questionnaire = defaultQuestionnaire
    , template = defaultTemplate
    , submission = defaultSubmission
    , owl = defaultOwl
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
    { defaultRole = _USER_ROLE_DATA_STEWARD
    , internal = defaultAuthInternal
    , external = defaultAuthExternal
    }

defaultAuthInternal :: AppConfigAuthInternal
defaultAuthInternal = AppConfigAuthInternal {registration = SimpleFeature True}

defaultAuthExternal :: AppConfigAuthExternal
defaultAuthExternal = AppConfigAuthExternal {services = []}

defaultPrivacyAndSupport :: AppConfigPrivacyAndSupport
defaultPrivacyAndSupport =
  AppConfigPrivacyAndSupport
    { privacyUrl = Nothing
    , termsOfServiceUrl = Nothing
    , supportEmail = Nothing
    , supportRepositoryName = Nothing
    , supportRepositoryUrl = Nothing
    }

defaultDashboard :: AppConfigDashboard
defaultDashboard =
  AppConfigDashboard
    { dashboardType = RoleBasedDashboardType
    , welcomeWarning = Nothing
    , welcomeInfo = Nothing
    }

defaultLookAndFeel :: AppConfigLookAndFeel
defaultLookAndFeel =
  AppConfigLookAndFeel
    { appTitle = Nothing
    , appTitleShort = Nothing
    , customMenuLinks = []
    , loginInfo = Nothing
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

defaultTemplate :: AppConfigTemplate
defaultTemplate = AppConfigTemplate {recommendedTemplateId = Nothing}

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

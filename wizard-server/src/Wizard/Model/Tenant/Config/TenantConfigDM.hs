module Wizard.Model.Tenant.Config.TenantConfigDM where

import qualified Data.UUID as U

import Shared.Common.Model.Config.SimpleFeature
import Shared.Common.Util.Date
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Config.TenantConfigSubmission
import Wizard.Model.User.User
import WizardLib.Public.Model.Tenant.Config.TenantConfig

defaultTenantConfig :: TenantConfig
defaultTenantConfig =
  TenantConfig
    { uuid = U.nil
    , organization = defaultOrganization
    , authentication = defaultAuth
    , privacyAndSupport = defaultPrivacyAndSupport
    , dashboardAndLoginScreen = defaultDashboardAndLoginScreen
    , lookAndFeel = defaultLookAndFeel
    , registry = defaultRegistry
    , knowledgeModel = defaultKnowledgeModel
    , questionnaire = defaultQuestionnaire
    , owl = defaultOwl
    , mailConfigUuid = Nothing
    , aiAssistant = defaultAiAssistant
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

defaultOrganization :: TenantConfigOrganization
defaultOrganization =
  TenantConfigOrganization
    { name = "My Organization"
    , description = "My description"
    , organizationId = "organization"
    , affiliations = []
    }

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
defaultAuthExternal = TenantConfigAuthExternal {services = []}

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
    { dashboardType = RoleBasedDashboardType
    , announcements = []
    , loginInfo = Nothing
    , loginInfoSidebar = Nothing
    }

defaultLookAndFeel :: TenantConfigLookAndFeel
defaultLookAndFeel =
  TenantConfigLookAndFeel
    { appTitle = Nothing
    , appTitleShort = Nothing
    , customMenuLinks = []
    , logoUrl = Nothing
    , primaryColor = Nothing
    , illustrationsColor = Nothing
    }

defaultRegistry :: TenantConfigRegistry
defaultRegistry = TenantConfigRegistry {enabled = False, token = ""}

defaultKnowledgeModel :: TenantConfigKnowledgeModel
defaultKnowledgeModel =
  TenantConfigKnowledgeModel
    { public = defaultKnowledgeModelPublic
    , integrationConfig = ""
    }

defaultKnowledgeModelPublic :: TenantConfigKnowledgeModelPublic
defaultKnowledgeModelPublic =
  TenantConfigKnowledgeModelPublic
    { enabled = False
    , packages = []
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
    , anonymousEnabled = True
    }

defaultQuestionnaireProjectTagging :: TenantConfigQuestionnaireProjectTagging
defaultQuestionnaireProjectTagging =
  TenantConfigQuestionnaireProjectTagging
    { enabled = True
    , tags = []
    }

defaultFeedback :: TenantConfigQuestionnaireFeedback
defaultFeedback =
  TenantConfigQuestionnaireFeedback
    { enabled = False
    , token = ""
    , owner = ""
    , repo = ""
    }

defaultSubmission :: TenantConfigSubmission
defaultSubmission =
  TenantConfigSubmission
    { tenantUuid = U.nil
    , enabled = False
    , services = []
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
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

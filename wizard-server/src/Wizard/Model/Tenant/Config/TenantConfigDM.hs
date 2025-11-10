module Wizard.Model.Tenant.Config.TenantConfigDM where

import qualified Data.UUID as U

import Shared.Common.Model.Config.SimpleFeature
import Shared.Common.Util.Date
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User

defaultOrganization :: TenantConfigOrganization
defaultOrganization =
  TenantConfigOrganization
    { tenantUuid = U.nil
    , name = "My Organization"
    , description = "My description"
    , organizationId = "organization"
    , affiliations = []
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

defaultAuthentication :: TenantConfigAuthentication
defaultAuthentication =
  TenantConfigAuthentication
    { tenantUuid = U.nil
    , defaultRole = _USER_ROLE_RESEARCHER
    , internal = defaultAuthenticationInternal
    , external = defaultAuthenticationExternal
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

defaultAuthenticationInternal :: TenantConfigAuthenticationInternal
defaultAuthenticationInternal = TenantConfigAuthenticationInternal {registration = SimpleFeature True, twoFactorAuth = defaultAuthenticationInternalTwoFactorAuth}

defaultAuthenticationInternalTwoFactorAuth :: TenantConfigAuthenticationInternalTwoFactorAuth
defaultAuthenticationInternalTwoFactorAuth =
  TenantConfigAuthenticationInternalTwoFactorAuth
    { enabled = False
    , codeLength = 6
    , expiration = 600
    }

defaultAuthenticationExternal :: TenantConfigAuthenticationExternal
defaultAuthenticationExternal = TenantConfigAuthenticationExternal {services = []}

defaultPrivacyAndSupport :: TenantConfigPrivacyAndSupport
defaultPrivacyAndSupport =
  TenantConfigPrivacyAndSupport
    { tenantUuid = U.nil
    , privacyUrl = Nothing
    , termsOfServiceUrl = Nothing
    , supportEmail = Nothing
    , supportSiteName = Nothing
    , supportSiteUrl = Nothing
    , supportSiteIcon = Nothing
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

defaultDashboardAndLoginScreen :: TenantConfigDashboardAndLoginScreen
defaultDashboardAndLoginScreen =
  TenantConfigDashboardAndLoginScreen
    { tenantUuid = U.nil
    , dashboardType = RoleBasedDashboardType
    , announcements = []
    , loginInfo = Nothing
    , loginInfoSidebar = Nothing
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

defaultRegistry :: TenantConfigRegistry
defaultRegistry =
  TenantConfigRegistry
    { tenantUuid = U.nil
    , enabled = False
    , token = ""
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

defaultKnowledgeModel :: TenantConfigKnowledgeModel
defaultKnowledgeModel =
  TenantConfigKnowledgeModel
    { tenantUuid = U.nil
    , public = defaultKnowledgeModelPublic
    , integrationConfig = ""
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

defaultKnowledgeModelPublic :: TenantConfigKnowledgeModelPublic
defaultKnowledgeModelPublic =
  TenantConfigKnowledgeModelPublic
    { enabled = False
    , knowledgeModelPackages = []
    }

defaultQuestionnaire :: TenantConfigQuestionnaire
defaultQuestionnaire =
  TenantConfigQuestionnaire
    { tenantUuid = U.nil
    , questionnaireVisibility = defaultQuestionnaireVisibility
    , questionnaireSharing = defaultQuestionnaireSharing
    , questionnaireCreation = TemplateAndCustomQuestionnaireCreation
    , projectTagging = defaultQuestionnaireProjectTagging
    , summaryReport = SimpleFeature True
    , feedback = defaultFeedback
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
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
    { tenantUuid = U.nil
    , enabled = False
    , name = ""
    , organizationId = ""
    , kmId = ""
    , version = ""
    , previousKnowledgeModelPackageId = Nothing
    , rootElement = ""
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

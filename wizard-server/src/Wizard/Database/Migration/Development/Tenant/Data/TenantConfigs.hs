module Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs where

import qualified Data.Map.Strict as M

import Shared.Common.Model.Common.SensitiveData
import Shared.Common.Model.Config.SimpleFeature
import Shared.Common.Util.Date
import Shared.OpenId.Database.Migration.Development.OpenId.Data.OpenIds
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Config.TenantConfigEM ()
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import Wizard.Service.Tenant.Config.ConfigMapper
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
    , authentication = defaultAuthentication
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
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

defaultTenantConfigChangeDto :: TenantConfigChangeDTO
defaultTenantConfigChangeDto = toChangeDTO defaultOrganizationChangeDto defaultAuthenticationChangeDto defaultPrivacyAndSupportChangeDto defaultDashboardAndLoginScreenChangeDto defaultLookAndFeelChangeDto defaultRegistryChangeDto defaultKnowledgeModelChangeDto defaultQuestionnaireChangeDto defaultSubmissionChangeDto

defaultOrganization :: TenantConfigOrganization
defaultOrganization = fromOrganizationChangeDTO defaultOrganizationChangeDto defaultTenant.uuid (dt' 2018 1 20) (dt' 2018 1 20)

defaultOrganizationChangeDto :: TenantConfigOrganizationChangeDTO
defaultOrganizationChangeDto =
  TenantConfigOrganizationChangeDTO
    { name = "Organization Amsterdam"
    , description = "Some description of Organization Amsterdam"
    , organizationId = "org.nl.amsterdam"
    , affiliations = []
    }

defaultAuthentication :: TenantConfigAuthentication
defaultAuthentication = fromAuthenticationChangeDTO defaultAuthenticationChangeDto defaultTenant.uuid (dt' 2018 1 20) (dt' 2018 1 20)

defaultAuthenticationEncrypted :: TenantConfigAuthentication
defaultAuthenticationEncrypted = process defaultSecret defaultAuthentication

defaultAuthenticationChangeDto :: TenantConfigAuthenticationChangeDTO
defaultAuthenticationChangeDto =
  TenantConfigAuthenticationChangeDTO
    { defaultRole = _USER_ROLE_RESEARCHER
    , internal = defaultAuthenticationInternal
    , external = defaultAuthenticationExternalChangeDto
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
defaultAuthenticationExternal = TenantConfigAuthenticationExternal {services = [defaultAuthenticationExternalService]}

defaultAuthenticationExternalChangeDto :: TenantConfigAuthenticationExternalChangeDTO
defaultAuthenticationExternalChangeDto = TenantConfigAuthenticationExternalChangeDTO {services = [defaultAuthenticationExternalServiceChangeDto]}

defaultAuthenticationExternalService :: TenantConfigAuthenticationExternalService
defaultAuthenticationExternalService = fromAuthenticationExternalServiceChangeDTO defaultAuthenticationExternalServiceChangeDto defaultTenant.uuid (dt' 2018 1 20) (dt' 2018 1 20)

defaultAuthExternalServiceEncrypted :: TenantConfigAuthenticationExternalService
defaultAuthExternalServiceEncrypted = process defaultSecret defaultAuthenticationExternalService

defaultAuthenticationExternalServiceChangeDto :: TenantConfigAuthenticationExternalServiceChangeDTO
defaultAuthenticationExternalServiceChangeDto =
  TenantConfigAuthenticationExternalServiceChangeDTO
    { aId = "google"
    , name = "Google"
    , url = "https://accounts.google.com"
    , clientId = "32559869123-a98908094.apps.googleusercontent.com"
    , clientSecret = "sad89089023"
    , parameters = [openIdClientDefinitionParameter]
    , style = openIdClientDefinitionStyle
    }

defaultPrivacyAndSupport :: TenantConfigPrivacyAndSupport
defaultPrivacyAndSupport = fromPrivacyAndSupportChangeDTO defaultPrivacyAndSupportChangeDto defaultTenant.uuid (dt' 2018 1 20) (dt' 2018 1 20)

defaultPrivacyAndSupportChangeDto :: TenantConfigPrivacyAndSupportChangeDTO
defaultPrivacyAndSupportChangeDto =
  TenantConfigPrivacyAndSupportChangeDTO
    { privacyUrl = Nothing
    , termsOfServiceUrl = Nothing
    , supportEmail = Nothing
    , supportSiteName = Nothing
    , supportSiteUrl = Nothing
    , supportSiteIcon = Nothing
    }

defaultDashboardAndLoginScreen :: TenantConfigDashboardAndLoginScreen
defaultDashboardAndLoginScreen = fromDashboardAndLoginScreenChangeDTO defaultDashboardAndLoginScreenChangeDto defaultTenant.uuid (dt' 2018 1 20) (dt' 2018 1 20)

defaultDashboardAndLoginScreenChangeDto :: TenantConfigDashboardAndLoginScreenChangeDTO
defaultDashboardAndLoginScreenChangeDto =
  TenantConfigDashboardAndLoginScreenChangeDTO
    { dashboardType = WelcomeDashboardType
    , announcements = [defaultDashboardAndLoginScreenAnnouncementChangeDto]
    , loginInfo = Nothing
    , loginInfoSidebar = Nothing
    }

defaultRegistry :: TenantConfigRegistry
defaultRegistry = fromRegistryChangeDTO defaultRegistryChangeDto defaultTenant.uuid (dt' 2018 1 20) (dt' 2018 1 20)

defaultRegistryEncrypted :: TenantConfigRegistry
defaultRegistryEncrypted = process defaultSecret defaultRegistry

defaultRegistryChangeDto :: TenantConfigRegistryChangeDTO
defaultRegistryChangeDto =
  TenantConfigRegistryChangeDTO
    { enabled = True
    , token = "GlobalToken"
    }

defaultKnowledgeModel :: TenantConfigKnowledgeModel
defaultKnowledgeModel = fromKnowledgeModelChangeDTO defaultKnowledgeModelChangeDto defaultTenant.uuid (dt' 2018 1 20) (dt' 2018 1 20)

defaultKnowledgeModelEncrypted :: TenantConfigKnowledgeModel
defaultKnowledgeModelEncrypted = process defaultSecret defaultKnowledgeModel

defaultKnowledgeModelChangeDto :: TenantConfigKnowledgeModelChangeDTO
defaultKnowledgeModelChangeDto =
  TenantConfigKnowledgeModelChangeDTO
    { public = defaultKnowledgeModelPublicChangeDto
    , integrationConfig =
        "ontologyPortal: \n\
        \  path: ontology-portal.json \n\
        \bioPortal: \n\
        \  path: bio-portal.json "
    }

defaultKnowledgeModelPublic :: TenantConfigKnowledgeModelPublic
defaultKnowledgeModelPublic =
  TenantConfigKnowledgeModelPublic
    { enabled = defaultKnowledgeModelPublicChangeDto.enabled
    , packages = zipWith (\i p -> fromKnowledgeModelPublicPackagePatternChangeDTO p defaultTenant.uuid i (dt' 2018 1 20) (dt' 2018 1 20)) [0 ..] defaultKnowledgeModelPublicChangeDto.packages
    }

defaultKnowledgeModelPublicChangeDto :: TenantConfigKnowledgeModelPublicChangeDTO
defaultKnowledgeModelPublicChangeDto =
  TenantConfigKnowledgeModelPublicChangeDTO
    { enabled = True
    , packages = [packagePatternGlobal]
    }

defaultKnowledgeModelPublicPackagePattern :: TenantConfigKnowledgeModelPublicPackagePattern
defaultKnowledgeModelPublicPackagePattern =
  TenantConfigKnowledgeModelPublicPackagePattern
    { tenantUuid = defaultTenant.uuid
    , position = 0
    , orgId = Just "global"
    , kmId = Just "core"
    , minVersion = Just "1.0.0"
    , maxVersion = Just "1.0.0"
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

defaultQuestionnaire :: TenantConfigQuestionnaire
defaultQuestionnaire = fromQuestionnaireChangeDTO defaultQuestionnaireChangeDto defaultTenant.uuid (dt' 2018 1 20) (dt' 2018 1 20)

defaultQuestionnaireEncrypted :: TenantConfigQuestionnaire
defaultQuestionnaireEncrypted = process defaultSecret defaultQuestionnaire

defaultQuestionnaireChangeDto :: TenantConfigQuestionnaireChangeDTO
defaultQuestionnaireChangeDto =
  TenantConfigQuestionnaireChangeDTO
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
defaultSubmission = fromSubmissionChangeDTO defaultSubmissionChangeDto defaultTenant.uuid (dt' 2018 1 20) (dt' 2018 1 20)

defaultSubmissionChangeDto :: TenantConfigSubmissionChangeDTO
defaultSubmissionChangeDto =
  TenantConfigSubmissionChangeDTO
    { enabled = True
    , services = [defaultSubmissionServiceChangeDto]
    }

defaultSubmissionChangeEmptyDto :: TenantConfigSubmissionChangeDTO
defaultSubmissionChangeEmptyDto =
  TenantConfigSubmissionChangeDTO
    { enabled = True
    , services = []
    }

defaultSubmissionService :: TenantConfigSubmissionService
defaultSubmissionService = fromSubmissionServiceChangeDTO defaultSubmissionServiceChangeDto defaultTenant.uuid (dt' 2018 1 20) (dt' 2018 1 20)

defaultSubmissionServiceChangeDto :: TenantConfigSubmissionServiceChangeDTO
defaultSubmissionServiceChangeDto =
  TenantConfigSubmissionServiceChangeDTO
    { sId = "mySubmissionServer"
    , name = "My Submission Server"
    , description = "Some description"
    , props = [defaultSubmissionServiceApiTokenProp, defaultSubmissionServiceSecretProp]
    , supportedFormats = [defaultSubmissionServiceSupportedFormatChangeDto]
    , request = defaultSubmissionServiceRequestChangeDto
    }

defaultSubmissionServiceApiTokenProp :: String
defaultSubmissionServiceApiTokenProp = "API Token"

defaultSubmissionServiceSecretProp :: String
defaultSubmissionServiceSecretProp = "Secret"

defaultSubmissionServiceSupportedFormat :: TenantConfigSubmissionServiceSupportedFormat
defaultSubmissionServiceSupportedFormat = fromSubmissionServiceSupportedFormatChangeDTO defaultSubmissionServiceSupportedFormatChangeDto defaultTenant.uuid defaultSubmissionServiceChangeDto.sId

defaultSubmissionServiceSupportedFormatChangeDto :: TenantConfigSubmissionServiceSupportedFormatChangeDTO
defaultSubmissionServiceSupportedFormatChangeDto =
  TenantConfigSubmissionServiceSupportedFormatChangeDTO
    { templateId = wizardDocumentTemplate.tId
    , formatUuid = formatJson.uuid
    }

defaultSubmissionServiceRequest :: TenantConfigSubmissionServiceRequest
defaultSubmissionServiceRequest = fromSubmissionServiceRequestChangeDTO defaultSubmissionServiceRequestChangeDto

defaultSubmissionServiceRequestChangeDto :: TenantConfigSubmissionServiceRequestChangeDTO
defaultSubmissionServiceRequestChangeDto =
  TenantConfigSubmissionServiceRequestChangeDTO
    { method = "GET"
    , url = "https://mockserver.ds-wizard.org/submission.json"
    , headers = M.fromList [("Api-Key", "${API Token}")]
    , multipart = defaultSubmissionServiceRequestMultipartChangeDto
    }

defaultSubmissionServiceRequestMultipart :: TenantConfigSubmissionServiceRequestMultipart
defaultSubmissionServiceRequestMultipart = fromSubmissionServiceRequestMultipartChangeDTO defaultSubmissionServiceRequestMultipartChangeDto

defaultSubmissionServiceRequestMultipartChangeDto :: TenantConfigSubmissionServiceRequestMultipartChangeDTO
defaultSubmissionServiceRequestMultipartChangeDto =
  TenantConfigSubmissionServiceRequestMultipartChangeDTO
    { enabled = False
    , fileName = "file"
    }

defaultOwl :: TenantConfigOwl
defaultOwl =
  TenantConfigOwl
    { tenantUuid = defaultTenant.uuid
    , enabled = False
    , name = ""
    , organizationId = ""
    , kmId = ""
    , version = ""
    , previousPackageId = Nothing
    , rootElement = ""
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

-- ------------------------------------------------------------
-- ------------------------------------------------------------
editedQuestionnaire :: TenantConfigQuestionnaire
editedQuestionnaire = fromQuestionnaireChangeDTO editedQuestionnaireChangeDto defaultTenant.uuid (dt' 2018 1 20) (dt' 2018 1 20)

editedQuestionnaireChangeDto :: TenantConfigQuestionnaireChangeDTO
editedQuestionnaireChangeDto = defaultQuestionnaireChangeDto {summaryReport = SimpleFeature False}

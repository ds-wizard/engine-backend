module Wizard.Api.Resource.Tenant.Config.TenantConfigJM where

import Data.Aeson

import Shared.Common.Api.Resource.Config.SimpleFeatureJM ()
import Shared.Common.Util.Aeson
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterJM ()
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePatternJM ()
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigJM ()

instance FromJSON TenantConfig where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfig where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigOrganization where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigOrganization where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigAuthentication where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigAuthentication where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigAuthenticationInternal where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigAuthenticationInternal where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigAuthenticationInternalTwoFactorAuth where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigAuthenticationInternalTwoFactorAuth where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigAuthenticationExternal where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigAuthenticationExternal where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigAuthenticationExternalService where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigAuthenticationExternalService where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigPrivacyAndSupport where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigPrivacyAndSupport where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigDashboardAndLoginScreen where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigDashboardAndLoginScreen where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigDashboardAndLoginScreenDashboardType

instance ToJSON TenantConfigDashboardAndLoginScreenDashboardType

instance FromJSON TenantConfigRegistry where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigRegistry where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigKnowledgeModel where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigKnowledgeModel where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigKnowledgeModelPublic where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigKnowledgeModelPublic where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigKnowledgeModelPublicPackagePattern where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigKnowledgeModelPublicPackagePattern where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigQuestionnaire where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigQuestionnaire where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigQuestionnaireVisibility where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigQuestionnaireVisibility where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigQuestionnaireSharing where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigQuestionnaireSharing where
  toJSON = genericToJSON jsonOptions

instance FromJSON QuestionnaireCreation

instance ToJSON QuestionnaireCreation

instance FromJSON TenantConfigQuestionnaireProjectTagging where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigQuestionnaireProjectTagging where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigQuestionnaireFeedback where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigQuestionnaireFeedback where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigSubmission where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigSubmission where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigSubmissionService where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigSubmissionService where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigSubmissionServiceSupportedFormat where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigSubmissionServiceSupportedFormat where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigSubmissionServiceRequest where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigSubmissionServiceRequest where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigSubmissionServiceRequestMultipart where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigSubmissionServiceRequestMultipart where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigOwl where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigOwl where
  toJSON = genericToJSON jsonOptions

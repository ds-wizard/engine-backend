module Wizard.Api.Handler.Swagger.Api where

import Data.Swagger
import Servant
import Servant.Swagger
import Servant.Swagger.UI

import RegistryLib.Api.Resource.Organization.OrganizationSM ()
import RegistryLib.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Common.Api.Resource.Common.AesonSM ()
import Shared.Common.Api.Resource.Common.FileSM ()
import Shared.Common.Api.Resource.Common.PageMetadataSM ()
import Shared.Common.Api.Resource.Dev.DevExecutionResultSM ()
import Shared.Common.Api.Resource.Dev.DevExecutionSM ()
import Shared.Common.Api.Resource.Dev.DevSectionSM ()
import Shared.Common.Api.Resource.Info.InfoSM ()
import Shared.Component.Api.Resource.Component.ComponentSM ()
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandChangeSM ()
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Shared.Prefab.Api.Resource.Prefab.PrefabSM ()
import Wizard.Api.Handler.Api
import Wizard.Api.Resource.ActionKey.ActionKeyTypeSM ()
import Wizard.Api.Resource.Auth.AuthConsentSM ()
import Wizard.Api.Resource.Branch.BranchChangeSM ()
import Wizard.Api.Resource.Branch.BranchCreateSM ()
import Wizard.Api.Resource.Branch.BranchDetailSM ()
import Wizard.Api.Resource.Branch.BranchStateSM ()
import Wizard.Api.Resource.Common.PageSM ()
import Wizard.Api.Resource.Config.ClientConfigSM ()
import Wizard.Api.Resource.Document.DocumentCreateSM ()
import Wizard.Api.Resource.Document.DocumentSM ()
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeSM ()
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateSM ()
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetSM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeSM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailSM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplatePhaseSM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeSM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateSM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeSM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataSM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailSM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListSM ()
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeSM ()
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileListSM ()
import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteSM ()
import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveSM ()
import Wizard.Api.Resource.Feedback.FeedbackCreateSM ()
import Wizard.Api.Resource.Feedback.FeedbackSM ()
import Wizard.Api.Resource.File.FileCreateSM ()
import Wizard.Api.Resource.Locale.LocaleChangeSM ()
import Wizard.Api.Resource.Locale.LocaleCreateSM ()
import Wizard.Api.Resource.Locale.LocaleDetailSM ()
import Wizard.Api.Resource.Locale.LocaleSM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateSM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictSM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateSM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateSM ()
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeSM ()
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateSM ()
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateSM ()
import Wizard.Api.Resource.Package.PackageChangeSM ()
import Wizard.Api.Resource.Package.PackageDetailSM ()
import Wizard.Api.Resource.Package.PackageSuggestionSM ()
import Wizard.Api.Resource.Package.Publish.PackagePublishBranchSM ()
import Wizard.Api.Resource.Package.Publish.PackagePublishMigrationSM ()
import Wizard.Api.Resource.PackageBundle.PackageBundleFileSM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailSM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadAssignedSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailPreviewSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailReportSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailSettingsSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSettingsChangeSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireShareChangeSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSimpleSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSuggestionSM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeSM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertSM ()
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionChangeSM ()
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionSM ()
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterChangeSM ()
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterSM ()
import Wizard.Api.Resource.Registry.RegistryConfirmationSM ()
import Wizard.Api.Resource.Registry.RegistryCreateSM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationSM ()
import Wizard.Api.Resource.Report.ReportSM ()
import Wizard.Api.Resource.Submission.SubmissionCreateSM ()
import Wizard.Api.Resource.Submission.SubmissionSM ()
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleSM ()
import Wizard.Api.Resource.TemporaryFile.TemporaryFileSM ()
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeSM ()
import Wizard.Api.Resource.Tenant.Config.TenantConfigSM ()
import Wizard.Api.Resource.Tenant.TenantChangeSM ()
import Wizard.Api.Resource.Tenant.TenantCreateSM ()
import Wizard.Api.Resource.Tenant.TenantDetailSM ()
import Wizard.Api.Resource.Tenant.TenantSM ()
import Wizard.Api.Resource.Typehint.TypehintRequestSM ()
import Wizard.Api.Resource.Typehint.TypehintSM ()
import Wizard.Api.Resource.User.UserChangeSM ()
import Wizard.Api.Resource.User.UserCreateSM ()
import Wizard.Api.Resource.User.UserPasswordSM ()
import Wizard.Api.Resource.User.UserProfileChangeSM ()
import Wizard.Api.Resource.User.UserProfileSM ()
import Wizard.Api.Resource.User.UserSM ()
import Wizard.Api.Resource.User.UserStateSM ()
import Wizard.Api.Resource.UserToken.ApiKeyCreateSM ()
import Wizard.Api.Resource.UserToken.AppKeyCreateSM ()
import Wizard.Api.Resource.UserToken.UserTokenListSM ()
import Wizard.Api.Resource.Websocket.QuestionnaireActionSM ()
import Wizard.Api.Resource.Websocket.WebsocketSM ()
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleSM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelChangeSM ()
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePhaseSM ()
import WizardLib.Public.Api.Resource.PersistentCommand.PersistentCommandListSM ()
import WizardLib.Public.Api.Resource.Tenant.Limit.TenantLimitBundleChangeSM ()
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageSM ()
import WizardLib.Public.Api.Resource.User.Group.UserGroupDetailSM ()
import WizardLib.Public.Api.Resource.UserToken.LoginSM ()
import WizardLib.Public.Api.Resource.UserToken.UserTokenSM ()

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

swagger :: Swagger
swagger =
  let s = toSwagger applicationApi
   in s
        { _swaggerInfo =
            s._swaggerInfo
              { _infoTitle = "Wizard API"
              , _infoDescription = Just "API specification for Wizard"
              , _infoVersion = "4.16.2"
              , _infoLicense =
                  Just $
                    License
                      { _licenseName = "Apache-2.0"
                      , _licenseUrl = Just . URL $ "https://raw.githubusercontent.com/ds-wizard/engine-backend/main/LICENSE.md"
                      }
              }
        , _swaggerBasePath = Just "/wizard-api"
        }

swaggerServer :: Server SwaggerAPI
swaggerServer = swaggerSchemaUIServer swagger

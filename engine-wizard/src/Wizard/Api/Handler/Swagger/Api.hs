module Wizard.Api.Handler.Swagger.Api where

import Data.Swagger
import Servant
import Servant.Swagger
import Servant.Swagger.UI

import Registry.Api.Resource.Organization.OrganizationSM ()
import Shared.Api.Resource.Common.FileSM ()
import Shared.Api.Resource.Common.PageMetadataSM ()
import Shared.Api.Resource.Component.ComponentSM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleSM ()
import Shared.Api.Resource.Info.InfoSM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeSM ()
import Shared.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Api.Resource.Package.PackagePhaseSM ()
import Wizard.Api.Api
import Wizard.Api.Resource.ActionKey.ActionKeySM ()
import Wizard.Api.Resource.App.AppChangeSM ()
import Wizard.Api.Resource.App.AppCreateSM ()
import Wizard.Api.Resource.App.AppDetailSM ()
import Wizard.Api.Resource.App.AppSM ()
import Wizard.Api.Resource.BookReference.BookReferenceSM ()
import Wizard.Api.Resource.Branch.BranchChangeSM ()
import Wizard.Api.Resource.Branch.BranchCreateSM ()
import Wizard.Api.Resource.Branch.BranchDetailSM ()
import Wizard.Api.Resource.Branch.BranchStateSM ()
import Wizard.Api.Resource.Common.AesonSM ()
import Wizard.Api.Resource.Common.PageSM ()
import Wizard.Api.Resource.Config.AppConfigChangeSM ()
import Wizard.Api.Resource.Config.AppConfigSM ()
import Wizard.Api.Resource.Config.ClientConfigSM ()
import Wizard.Api.Resource.Dev.DevExecutionResultSM ()
import Wizard.Api.Resource.Dev.DevExecutionSM ()
import Wizard.Api.Resource.Dev.DevSM ()
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
import Wizard.Api.Resource.Feedback.FeedbackCreateSM ()
import Wizard.Api.Resource.Feedback.FeedbackSM ()
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
import Wizard.Api.Resource.PersistentCommand.PersistentCommandChangeSM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailSM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Wizard.Api.Resource.Plan.AppPlanChangeSM ()
import Wizard.Api.Resource.Plan.AppPlanSM ()
import Wizard.Api.Resource.Prefab.PrefabSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSimpleSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSuggestionSM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeSM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertSM ()
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
import Wizard.Api.Resource.Typehint.TypehintRequestSM ()
import Wizard.Api.Resource.Typehint.TypehintSM ()
import Wizard.Api.Resource.Usage.UsageSM ()
import Wizard.Api.Resource.User.UserChangeSM ()
import Wizard.Api.Resource.User.UserCreateSM ()
import Wizard.Api.Resource.User.UserPasswordSM ()
import Wizard.Api.Resource.User.UserProfileChangeSM ()
import Wizard.Api.Resource.User.UserProfileSM ()
import Wizard.Api.Resource.User.UserSM ()
import Wizard.Api.Resource.User.UserStateSM ()
import Wizard.Api.Resource.UserToken.ApiKeyCreateSM ()
import Wizard.Api.Resource.UserToken.LoginSM ()
import Wizard.Api.Resource.UserToken.UserTokenListSM ()
import Wizard.Api.Resource.UserToken.UserTokenSM ()
import Wizard.Api.Resource.Websocket.QuestionnaireActionSM ()
import Wizard.Api.Resource.Websocket.WebsocketSM ()

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

swagger :: Swagger
swagger =
  let s = toSwagger applicationApi
   in s
        { _swaggerInfo =
            s._swaggerInfo
              { _infoTitle = "Wizard API"
              , _infoDescription = Just "API specification for Wizard"
              , _infoVersion = "3.22.3"
              , _infoLicense =
                  Just $
                    License
                      { _licenseName = "Apache-2.0"
                      , _licenseUrl = Just . URL $ "https://raw.githubusercontent.com/ds-wizard/engine-backend/main/LICENSE.md"
                      }
              }
        }

swaggerServer :: Server SwaggerAPI
swaggerServer = swaggerSchemaUIServer swagger

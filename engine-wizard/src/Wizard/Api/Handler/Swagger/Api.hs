module Wizard.Api.Handler.Swagger.Api where

import Control.Lens
import Data.Swagger
import Servant
import Servant.Swagger
import Servant.Swagger.UI

import Registry.Api.Resource.Organization.OrganizationSM ()
import Shared.Api.Resource.Common.PageMetadataSM ()
import Shared.Api.Resource.Info.InfoSM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeSM ()
import Shared.Api.Resource.Multipart.MultiPartSM ()
import Shared.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Api.Resource.Template.TemplateSM ()
import Wizard.Api.Api
import Wizard.Api.Resource.ActionKey.ActionKeySM ()
import Wizard.Api.Resource.BookReference.BookReferenceSM ()
import Wizard.Api.Resource.Branch.BranchChangeSM ()
import Wizard.Api.Resource.Branch.BranchCreateSM ()
import Wizard.Api.Resource.Branch.BranchDetailSM ()
import Wizard.Api.Resource.Branch.BranchSM ()
import Wizard.Api.Resource.Branch.BranchStateSM ()
import Wizard.Api.Resource.Common.PageSM ()
import Wizard.Api.Resource.Config.AppConfigChangeSM ()
import Wizard.Api.Resource.Config.AppConfigSM ()
import Wizard.Api.Resource.Config.ClientConfigSM ()
import Wizard.Api.Resource.Document.DocumentCreateSM ()
import Wizard.Api.Resource.Document.DocumentSM ()
import Wizard.Api.Resource.Feedback.FeedbackCreateSM ()
import Wizard.Api.Resource.Feedback.FeedbackSM ()
import Wizard.Api.Resource.Level.LevelSM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateSM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictSM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateSM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateSM ()
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeSM ()
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateSM ()
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateSM ()
import Wizard.Api.Resource.Package.PackageDetailSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeSM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertSM ()
import Wizard.Api.Resource.Registry.RegistryConfirmationSM ()
import Wizard.Api.Resource.Registry.RegistryCreateSM ()
import Wizard.Api.Resource.Report.ReportSM ()
import Wizard.Api.Resource.Submission.SubmissionCreateSM ()
import Wizard.Api.Resource.Submission.SubmissionSM ()
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleSM ()
import Wizard.Api.Resource.Template.File.TemplateFileChangeSM ()
import Wizard.Api.Resource.Template.TemplateChangeSM ()
import Wizard.Api.Resource.Template.TemplateDetailSM ()
import Wizard.Api.Resource.Template.TemplateSimpleSM ()
import Wizard.Api.Resource.Token.TokenCreateSM ()
import Wizard.Api.Resource.Token.TokenSM ()
import Wizard.Api.Resource.Typehint.TypehintRequestSM ()
import Wizard.Api.Resource.Typehint.TypehintSM ()
import Wizard.Api.Resource.User.UserChangeSM ()
import Wizard.Api.Resource.User.UserCreateSM ()
import Wizard.Api.Resource.User.UserPasswordSM ()
import Wizard.Api.Resource.User.UserProfileChangeSM ()
import Wizard.Api.Resource.User.UserProfileSM ()
import Wizard.Api.Resource.User.UserSM ()
import Wizard.Api.Resource.User.UserStateSM ()
import Wizard.Api.Resource.Version.VersionSM ()
import Wizard.Api.Resource.Websocket.QuestionnaireActionSM ()
import Wizard.Api.Resource.Websocket.WebsocketSM ()

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

swagger :: Swagger
swagger = toSwagger appApi & info . title .~ "Wizard API" & info . description ?~ "API specification for Wizard"

swaggerServer :: Server SwaggerAPI
swaggerServer = swaggerSchemaUIServer swagger

module Wizard.Api.Api where

import Servant

import Wizard.Api.Handler.ActionKey.Api
import Wizard.Api.Handler.App.Api
import Wizard.Api.Handler.App.Plan.Api
import Wizard.Api.Handler.Auth.Api
import Wizard.Api.Handler.BookReference.Api
import Wizard.Api.Handler.Branch.Api
import Wizard.Api.Handler.Config.Api
import Wizard.Api.Handler.Dev.Api
import Wizard.Api.Handler.Document.Api
import Wizard.Api.Handler.DocumentTemplate.Api
import Wizard.Api.Handler.DocumentTemplateDraft.Api
import Wizard.Api.Handler.DocumentTemplateDraft.Asset.Api
import Wizard.Api.Handler.DocumentTemplateDraft.File.Api
import Wizard.Api.Handler.Domain.Api
import Wizard.Api.Handler.Feedback.Api
import Wizard.Api.Handler.Info.Api
import Wizard.Api.Handler.KnowledgeModel.Api
import Wizard.Api.Handler.Locale.Api
import Wizard.Api.Handler.Migration.Api
import Wizard.Api.Handler.Package.Api
import Wizard.Api.Handler.PersistentCommand.Api
import Wizard.Api.Handler.Prefab.Api
import Wizard.Api.Handler.Questionnaire.Api
import Wizard.Api.Handler.Questionnaire.Event.Api
import Wizard.Api.Handler.Questionnaire.Version.Api
import Wizard.Api.Handler.QuestionnaireImporter.Api
import Wizard.Api.Handler.Registry.Api
import Wizard.Api.Handler.Submission.Api
import Wizard.Api.Handler.Token.Api
import Wizard.Api.Handler.Typehint.Api
import Wizard.Api.Handler.Usage.Api
import Wizard.Api.Handler.User.Api
import Wizard.Model.Context.BaseContext

type ApplicationAPI =
  ActionKeyAPI
    :<|> AppAPI
    :<|> AppPlanAPI
    :<|> AuthAPI
    :<|> BookReferenceAPI
    :<|> BranchAPI
    :<|> ConfigAPI
    :<|> DevAPI
    :<|> DocumentTemplateAPI
    :<|> DocumentTemplateDraftAPI
    :<|> DocumentTemplateAssetAPI
    :<|> DocumentTemplateFileAPI
    :<|> DocumentAPI
    :<|> DomainAPI
    :<|> FeedbackAPI
    :<|> InfoAPI
    :<|> KnowledgeModelAPI
    :<|> LocaleAPI
    :<|> MigrationAPI
    :<|> PackageAPI
    :<|> PersistentCommandAPI
    :<|> PrefabAPI
    :<|> QuestionnaireAPI
    :<|> QuestionnaireEventAPI
    :<|> QuestionnaireVersionAPI
    :<|> QuestionnaireImporterAPI
    :<|> RegistryAPI
    :<|> SubmissionAPI
    :<|> TokenAPI
    :<|> TypehintAPI
    :<|> UsageAPI
    :<|> UserAPI

applicationApi :: Proxy ApplicationAPI
applicationApi = Proxy

applicationServer :: ServerT ApplicationAPI BaseContextM
applicationServer =
  actionKeyServer
    :<|> appServer
    :<|> appPlanServer
    :<|> authServer
    :<|> bookReferenceServer
    :<|> branchServer
    :<|> configServer
    :<|> devServer
    :<|> documentTemplateServer
    :<|> documentTemplateDraftServer
    :<|> documentTemplateAssetServer
    :<|> documentTemplateFileServer
    :<|> documentServer
    :<|> domainServer
    :<|> feedbackServer
    :<|> infoServer
    :<|> knowledgeModelServer
    :<|> localeServer
    :<|> migrationServer
    :<|> packageServer
    :<|> persistentCommandServer
    :<|> prefabServer
    :<|> questionnaireServer
    :<|> questionnaireEventServer
    :<|> questionnaireVersionServer
    :<|> questionnaireImporterServer
    :<|> registryServer
    :<|> submissionServer
    :<|> tokenServer
    :<|> typehintServer
    :<|> usageServer
    :<|> userServer

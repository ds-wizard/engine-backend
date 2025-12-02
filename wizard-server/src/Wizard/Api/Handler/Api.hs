module Wizard.Api.Handler.Api where

import Servant

import Wizard.Api.Handler.ActionKey.Api
import Wizard.Api.Handler.ApiKey.Api
import Wizard.Api.Handler.AppKey.Api
import Wizard.Api.Handler.Auth.Api
import Wizard.Api.Handler.CommentThread.Api
import Wizard.Api.Handler.Config.Api
import Wizard.Api.Handler.Dev.Api
import Wizard.Api.Handler.Document.Api
import Wizard.Api.Handler.DocumentTemplate.Api
import Wizard.Api.Handler.DocumentTemplateDraft.Api
import Wizard.Api.Handler.DocumentTemplateDraft.Asset.Api
import Wizard.Api.Handler.DocumentTemplateDraft.File.Api
import Wizard.Api.Handler.DocumentTemplateDraft.Folder.Api
import Wizard.Api.Handler.Domain.Api
import Wizard.Api.Handler.ExternalLink.Api
import Wizard.Api.Handler.Feedback.Api
import Wizard.Api.Handler.Info.Api
import Wizard.Api.Handler.KnowledgeModel.Api
import Wizard.Api.Handler.KnowledgeModelEditor.Api
import Wizard.Api.Handler.KnowledgeModelPackage.Api
import Wizard.Api.Handler.KnowledgeModelSecret.Api
import Wizard.Api.Handler.Locale.Api
import Wizard.Api.Handler.Migration.Api
import Wizard.Api.Handler.PersistentCommand.Api
import Wizard.Api.Handler.Prefab.Api
import Wizard.Api.Handler.Questionnaire.Api
import Wizard.Api.Handler.Questionnaire.Event.Api
import Wizard.Api.Handler.Questionnaire.Version.Api
import Wizard.Api.Handler.QuestionnaireAction.Api
import Wizard.Api.Handler.QuestionnaireFile.Api
import Wizard.Api.Handler.QuestionnaireImporter.Api
import Wizard.Api.Handler.Registry.Api
import Wizard.Api.Handler.Submission.Api
import Wizard.Api.Handler.Tenant.Api
import Wizard.Api.Handler.Token.Api
import Wizard.Api.Handler.TypeHint.Api
import Wizard.Api.Handler.User.Api
import Wizard.Api.Handler.UserGroup.Api
import Wizard.Model.Context.BaseContext

type ApplicationAPI =
  ActionKeyAPI
    :<|> ApiKeyAPI
    :<|> AppKeyAPI
    :<|> AuthAPI
    :<|> CommentThreadAPI
    :<|> ConfigAPI
    :<|> DevAPI
    :<|> DocumentTemplateAPI
    :<|> DocumentTemplateDraftAPI
    :<|> DocumentTemplateFolderAPI
    :<|> DocumentTemplateAssetAPI
    :<|> DocumentTemplateFileAPI
    :<|> DocumentAPI
    :<|> DomainAPI
    :<|> ExternalLinkAPI
    :<|> FeedbackAPI
    :<|> InfoAPI
    :<|> KnowledgeModelAPI
    :<|> KnowledgeModelEditorAPI
    :<|> KnowledgeModelPackageAPI
    :<|> KnowledgeModelSecretAPI
    :<|> LocaleAPI
    :<|> MigrationAPI
    :<|> PersistentCommandAPI
    :<|> PrefabAPI
    :<|> QuestionnaireAPI
    :<|> QuestionnaireEventAPI
    :<|> QuestionnaireVersionAPI
    :<|> QuestionnaireFileAPI
    :<|> QuestionnaireActionAPI
    :<|> QuestionnaireImporterAPI
    :<|> RegistryAPI
    :<|> SubmissionAPI
    :<|> TenantAPI
    :<|> TokenAPI
    :<|> TypeHintAPI
    :<|> UserAPI
    :<|> UserGroupAPI

applicationApi :: Proxy ApplicationAPI
applicationApi = Proxy

applicationServer :: ServerT ApplicationAPI BaseContextM
applicationServer =
  actionKeyServer
    :<|> apiKeyServer
    :<|> appKeyServer
    :<|> authServer
    :<|> commentThreadServer
    :<|> configServer
    :<|> devServer
    :<|> documentTemplateServer
    :<|> documentTemplateDraftServer
    :<|> documentTemplateFolderServer
    :<|> documentTemplateAssetServer
    :<|> documentTemplateFileServer
    :<|> documentServer
    :<|> domainServer
    :<|> externalLinkServer
    :<|> feedbackServer
    :<|> infoServer
    :<|> knowledgeModelServer
    :<|> knowledgeModelEditorServer
    :<|> knowledgeModelPackageServer
    :<|> knowledgeModelSecretServer
    :<|> localeServer
    :<|> migrationServer
    :<|> persistentCommandServer
    :<|> prefabServer
    :<|> questionnaireServer
    :<|> questionnaireEventServer
    :<|> questionnaireVersionServer
    :<|> questionnaireFileServer
    :<|> questionnaireActionServer
    :<|> questionnaireImporterServer
    :<|> registryServer
    :<|> submissionServer
    :<|> tenantServer
    :<|> tokenServer
    :<|> typeHintServer
    :<|> userServer
    :<|> userGroupServer

module Wizard.Api.Api where

import Servant

import Wizard.Api.Handler.ActionKey.Api
import Wizard.Api.Handler.App.Api
import Wizard.Api.Handler.Auth.Api
import Wizard.Api.Handler.BookReference.Api
import Wizard.Api.Handler.Branch.Api
import Wizard.Api.Handler.Config.Api
import Wizard.Api.Handler.Dev.Api
import Wizard.Api.Handler.Document.Api
import Wizard.Api.Handler.Domain.Api
import Wizard.Api.Handler.Feedback.Api
import Wizard.Api.Handler.Info.Api
import Wizard.Api.Handler.KnowledgeModel.Api
import Wizard.Api.Handler.Migration.Api
import Wizard.Api.Handler.Package.Api
import Wizard.Api.Handler.PersistentCommand.Api
import Wizard.Api.Handler.Prefab.Api
import Wizard.Api.Handler.Questionnaire.Api
import Wizard.Api.Handler.QuestionnaireImporter.Api
import Wizard.Api.Handler.Registry.Api
import Wizard.Api.Handler.Submission.Api
import Wizard.Api.Handler.Template.Api
import Wizard.Api.Handler.Token.Api
import Wizard.Api.Handler.Typehint.Api
import Wizard.Api.Handler.Usage.Api
import Wizard.Api.Handler.User.Api
import Wizard.Api.Handler.Version.Api
import Wizard.Model.Context.BaseContext

type ApplicationAPI
   = ActionKeyAPI
     :<|> AppAPI
     :<|> AuthAPI
     :<|> BookReferenceAPI
     :<|> BranchAPI
     :<|> ConfigAPI
     :<|> DevAPI
     :<|> DocumentAPI
     :<|> DomainAPI
     :<|> FeedbackAPI
     :<|> InfoAPI
     :<|> KnowledgeModelAPI
     :<|> MigrationAPI
     :<|> PackageAPI
     :<|> PersistentCommandAPI
     :<|> PrefabAPI
     :<|> QuestionnaireAPI
     :<|> QuestionnaireImporterAPI
     :<|> RegistryAPI
     :<|> SubmissionAPI
     :<|> TemplateAPI
     :<|> TokenAPI
     :<|> TypehintAPI
     :<|> UsageAPI
     :<|> UserAPI
     :<|> VersionAPI

applicationApi :: Proxy ApplicationAPI
applicationApi = Proxy

applicationServer :: ServerT ApplicationAPI BaseContextM
applicationServer =
  actionKeyServer :<|> appServer :<|> authServer :<|> bookReferenceServer :<|> branchServer :<|> configServer :<|>
  devServer :<|>
  documentServer :<|>
  domainServer :<|>
  feedbackServer :<|>
  infoServer :<|>
  knowledgeModelServer :<|>
  migrationServer :<|>
  packageServer :<|>
  persistentCommandServer :<|>
  prefabServer :<|>
  questionnaireServer :<|>
  questionnaireImporterServer :<|>
  registryServer :<|>
  submissionServer :<|>
  templateServer :<|>
  tokenServer :<|>
  typehintServer :<|>
  usageServer :<|>
  userServer :<|>
  versionServer

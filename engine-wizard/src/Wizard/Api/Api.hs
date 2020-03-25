module Wizard.Api.Api where

import Servant

import Wizard.Api.Handler.ActionKey.Api
import Wizard.Api.Handler.BookReference.Api
import Wizard.Api.Handler.Branch.Api
import Wizard.Api.Handler.Config.Api
import Wizard.Api.Handler.Document.Api
import Wizard.Api.Handler.Feedback.Api
import Wizard.Api.Handler.IO.Api
import Wizard.Api.Handler.Info.Api
import Wizard.Api.Handler.KnowledgeModel.Api
import Wizard.Api.Handler.Level.Api
import Wizard.Api.Handler.Metric.Api
import Wizard.Api.Handler.Migration.Api
import Wizard.Api.Handler.Package.Api
import Wizard.Api.Handler.Questionnaire.Api
import Wizard.Api.Handler.Template.Api
import Wizard.Api.Handler.Token.Api
import Wizard.Api.Handler.Typehint.Api
import Wizard.Api.Handler.User.Api
import Wizard.Api.Handler.Version.Api
import Wizard.Model.Context.BaseContext

type AppAPI
   = ActionKeyAPI
     :<|> BookReferenceAPI
     :<|> BranchAPI
     :<|> ConfigAPI
     :<|> DocumentAPI
     :<|> FeedbackAPI
     :<|> IoAPI
     :<|> InfoAPI
     :<|> KnowledgeModelAPI
     :<|> LevelAPI
     :<|> MetricAPI
     :<|> MigrationAPI
     :<|> PackageAPI
     :<|> QuestionnaireAPI
     :<|> TemplateAPI
     :<|> TokenAPI
     :<|> TypehintAPI
     :<|> UserAPI
     :<|> VersionAPI

appApi :: Proxy AppAPI
appApi = Proxy

appServer :: ServerT AppAPI BaseContextM
appServer =
  actionKeyServer :<|> bookReferenceServer :<|> branchServer :<|> configServer :<|> documentServer :<|> feedbackServer :<|>
  ioServer :<|>
  infoServer :<|>
  knowledgeModelServer :<|>
  levelServer :<|>
  metricServer :<|>
  migrationServer :<|>
  packageServer :<|>
  questionnaireServer :<|>
  templateServer :<|>
  tokenServer :<|>
  typehintServer :<|>
  userServer :<|>
  versionServer

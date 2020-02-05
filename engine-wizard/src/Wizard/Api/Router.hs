module Wizard.Api.Router where

import Control.Lens ((^.))
import Data.Text.Lazy (Text)
import Network.HTTP.Types.Method (methodGet, methodPost, methodPut)
import Text.Regex
import Web.Scotty.Trans (ScottyT, defaultHandler, delete, get, middleware, notFound, post, put)

import LensesConfig
import Wizard.Api.Handler.ActionKey.ActionKeyHandler
import Wizard.Api.Handler.BookReference.BookReferenceHandler
import Wizard.Api.Handler.Branch.BranchHandler
import Wizard.Api.Handler.Common
import Wizard.Api.Handler.Config.ClientConfigHandler
import Wizard.Api.Handler.Document.DocumentHandler
import Wizard.Api.Handler.Feedback.FeedbackHandler
import Wizard.Api.Handler.IO.IOHandler
import Wizard.Api.Handler.Info.InfoHandler
import Wizard.Api.Handler.KnowledgeModel.KnowledgeModelHandler
import Wizard.Api.Handler.Level.LevelHandler
import Wizard.Api.Handler.Metric.MetricHandler
import Wizard.Api.Handler.Migration.KnowledgeModel.MigratorHandler
import Wizard.Api.Handler.Migration.Questionnaire.MigratorHandler
import Wizard.Api.Handler.Organization.OrganizationHandler
import Wizard.Api.Handler.Package.PackageHandler
import Wizard.Api.Handler.PublicQuestionnaire.PublicQuestionnaireHandler
import Wizard.Api.Handler.Questionnaire.QuestionnaireHandler
import Wizard.Api.Handler.Template.TemplateHandler
import Wizard.Api.Handler.Token.TokenHandler
import Wizard.Api.Handler.Typehint.TypehintHandler
import Wizard.Api.Handler.User.UserHandler
import Wizard.Api.Handler.Version.VersionHandler
import Wizard.Api.Middleware.AuthMiddleware
import Wizard.Api.Middleware.CORSMiddleware
import Wizard.Api.Middleware.LoggingMiddleware
import Wizard.Model.Context.BaseContext

unauthorizedEndpoints =
  [ (methodGet, mkRegex "^$")
  , (methodGet, mkRegex "^configuration$")
  , (methodPost, mkRegex "^tokens$")
  , (methodGet, mkRegex "^export/.*$")
  , (methodPost, mkRegex "^users")
  , (methodPut, mkRegex "^users/.*/state")
  , (methodPut, mkRegex "^users/.*/password")
  , (methodPut, mkRegex "^users/.*/password?hash=.*")
  , (methodPost, mkRegex "^action-keys$")
  , (methodGet, mkRegex "^questionnaires/public$")
  , (methodGet, mkRegex "^documents/.*/download")
  , (methodGet, mkRegex "^book-references/.*")
  , (methodGet, mkRegex "^feedbacks.*")
  , (methodPost, mkRegex "^feedbacks.*")
  ]

createEndpoints :: BaseContext -> ScottyT Text BaseContextM ()
createEndpoints context
   --------------------
   -- MIDDLEWARES
   --------------------
 = do
  middleware (loggingMiddleware (context ^. appConfig . general . environment))
  middleware corsMiddleware
  middleware (authMiddleware (context ^. appConfig) unauthorizedEndpoints)
   -- ------------------
   -- ERROR HANDLING
   -- ------------------
  defaultHandler internalServerErrorA
   -- ------------------
   -- INFO
   -- ------------------
  get "/" getInfoA
   --------------------
   -- TOKEN
   --------------------
  post "/tokens" postTokenA
   --------------------
   -- ORGANIZATION
   --------------------
  get "/organizations/current" getOrganizationCurrentA
  put "/organizations/current" putOrganizationCurrentA
   --------------------
   -- USER
   --------------------
  get "/users" getUsersA
  post "/users" postUsersA
  get "/users/current" getUserCurrentA
  get "/users/:userUuid" getUserA
  put "/users/current" putUserCurrentA
  put "/users/current/password" putUserCurrentPasswordA
  put "/users/:userUuid" putUserA
  put "/users/:userUuid/password" putUserPasswordA
  put "/users/:userUuid/state" changeUserStateA
  delete "/users/:userUuid" deleteUserA
  --  --------------------
  --  -- KNOWLEDGE MODEL
  --  --------------------
  -- Branch
  get "/branches" getBranchesA
  post "/branches" postBranchesA
  get "/branches/:branchUuid" getBranchA
  put "/branches/:branchUuid" putBranchA
  delete "/branches/:branchUuid" deleteBranchA
  -- Migrations
  get "/branches/:branchUuid/migrations/current" getMigrationsCurrentA
  post "/branches/:branchUuid/migrations/current" postMigrationsCurrentA
  delete "/branches/:branchUuid/migrations/current" deleteMigrationsCurrentA
  post "/branches/:branchUuid/migrations/current/conflict" postMigrationsCurrentConflictA
  -- Version
  put "/branches/:branchUuid/versions/:version" putVersionA
  -- Knowledge Model
  post "/knowledge-models/preview" postKnowledgeModelPreviewA
   --------------------
   -- PACKAGE
   --------------------
  get "/packages" getPackagesA
  post "/packages" postPackagesA
  get "/packages/:pkgId" getPackageA
  delete "/packages" deletePackagesA
  delete "/packages/:pkgId" deletePackageA
  post "/packages/:pkgId/pull" postPackagePullA
   --------------------
   -- ACTION KEY
   --------------------
  post "/action-keys" postActionKeysA
   --------------------
   -- IMPORT/EXPORT
   --------------------
  post "/import" importA
  get "/export/:pId" exportA
   --------------------
   -- QUESTIONNAIRE
   --------------------
  get "/questionnaires" getQuestionnairesA
  post "/questionnaires" postQuestionnairesA
  get "/questionnaires/public" getQuestionnairePublicA
  get "/questionnaires/:qtnUuid" getQuestionnaireA
  put "/questionnaires/:qtnUuid" putQuestionnaireA
  post "/questionnaires/:qtnUuid/report/preview" postQuestionnaireReportPreviewA
  get "/questionnaires/:qtnUuid/report" getQuestionnaireReportA
  delete "/questionnaires/:qtnUuid" deleteQuestionnaireA
   -- Migrations
  post "/questionnaires/:qtnUuid/migrations" postQuestionnaireMigrationsA
  get "/questionnaires/:qtnUuid/migrations/current" getQuestionnaireMigrationsCurrentA
  put "/questionnaires/:qtnUuid/migrations/current" putQuestionnaireMigrationsCurrentA
  delete "/questionnaires/:qtnUuid/migrations/current" deleteQuestionnaireMigrationsCurrentA
   --------------------
   -- TYPEHINTS
   --------------------
  post "/typehints" postTypehintsA
   --------------------
   -- TEMPLATE
   --------------------
  get "/templates" getTemplatesA
   --------------------
   -- DOCUMENT
   --------------------
  get "/documents" getDocumentsA
  post "/documents" postDocumentsA
  delete "/documents/:docUuid" deleteDocumentA
  get "/documents/:docUuid/download" downloadDocumentA
   --------------------
   -- BOOK REFERENCE
   --------------------
  get "/book-references/:brShortUuid" getBookReferenceA
   --------------------
   -- FEEDBACK
   --------------------
  get "/feedbacks" getFeedbacksA
  post "/feedbacks" postFeedbacksA
  get "/feedbacks/synchronization" getFeedbacksSynchronizationA
  get "/feedbacks/:fUuid" getFeedbackA
   --------------------
   -- METRIC
   --------------------
  get "/metrics" getMetricsA
   --------------------
   -- LEVEL
   --------------------
  get "/levels" getLevelsA
   --------------------
   -- LEVEL
   --------------------
  get "/configuration" getClientConfigA
   --------------------
   -- ERROR
   --------------------
  notFound notFoundA

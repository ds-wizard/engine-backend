module Api.Router where

import Control.Lens ((^.))
import Data.Text.Lazy (Text)
import Network.HTTP.Types.Method (methodGet, methodPost, methodPut)
import Text.Regex
import Web.Scotty.Trans
       (ScottyT, defaultHandler, delete, get, middleware, notFound, post,
        put)

import Api.Handler.ActionKey.ActionKeyHandler
import Api.Handler.BookReference.BookReferenceHandler
import Api.Handler.Branch.BranchHandler
import Api.Handler.Common
import Api.Handler.Config.ClientConfigHandler
import Api.Handler.Feedback.FeedbackHandler
import Api.Handler.IO.IOHandler
import Api.Handler.Info.InfoHandler
import Api.Handler.KnowledgeModel.KnowledgeModelHandler
import Api.Handler.Level.LevelHandler
import Api.Handler.Metric.MetricHandler
import Api.Handler.Migration.MigrationHandler
import Api.Handler.Organization.OrganizationHandler
import Api.Handler.Package.PackageHandler
import Api.Handler.PublicQuestionnaire.PublicQuestionnaireHandler
import Api.Handler.Questionnaire.QuestionnaireHandler
import Api.Handler.Template.TemplateHandler
import Api.Handler.Token.TokenHandler
import Api.Handler.Typehint.TypehintHandler
import Api.Handler.User.UserHandler
import Api.Handler.Version.VersionHandler
import Api.Middleware.AuthMiddleware
import Api.Middleware.CORSMiddleware
import Api.Middleware.LoggingMiddleware
import LensesConfig
import Model.Context.BaseContext

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
  , (methodGet, mkRegex "^questionnaires/.*/dmp")
  , (methodGet, mkRegex "^questionnaires/.*/dmp?format=.*$")
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
  get "/packages/unique" getUniquePackagesA
  get "/packages/:pkgId" getPackageA
  delete "/packages" deletePackagesA
  delete "/packages/:pkgId" deletePackageA
   --------------------
   -- ACTION KEY
   --------------------
  post "/action-keys" postActionKeysA
   --------------------
   -- IMPORT/EXPORT
   --------------------
  post "/import" importA
  get "/export/:kmbId" exportA
   --------------------
   -- QUESTIONNAIRE
   --------------------
  get "/questionnaires" getQuestionnairesA
  post "/questionnaires" postQuestionnairesA
  get "/questionnaires/public" getQuestionnairePublicA
  get "/questionnaires/:qtnUuid" getQuestionnaireA
  put "/questionnaires/:qtnUuid" putQuestionnaireA
  get "/questionnaires/:qtnUuid/dmp" getQuestionnaireDmpA
  post "/questionnaires/:qtnUuid/report/preview" postQuestionnaireReportPreviewA
  get "/questionnaires/:qtnUuid/report" getQuestionnaireReportA
  delete "/questionnaires/:qtnUuid" deleteQuestionnaireA
   --------------------
   -- TYPEHINTS
   --------------------
  post "/typehints" postTypehintsA
   --------------------
   -- TEMPLATE
   --------------------
  get "/templates" getTemplatesA
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

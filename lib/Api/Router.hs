module Api.Router where

import Control.Lens ((^.))
import Control.Monad.Logger
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (lift)
import Data.Text.Lazy (Text)
import Network.HTTP.Types.Method (methodGet, methodPost, methodPut)
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger
       (logStdout, logStdoutDev)
import Text.Regex
import Web.Scotty.Trans
       (ActionT, Options, ScottyT, defaultHandler, delete, get, json,
        jsonData, middleware, notFound, param, post, put, scottyOptsT,
        settings, showError, status, verbose)

import Api.Handler.ActionKey.ActionKeyHandler
import Api.Handler.Branch.BranchHandler
import Api.Handler.Common
import Api.Handler.Event.EventHandler
import Api.Handler.IO.IOHandler
import Api.Handler.Info.InfoHandler
import Api.Handler.KnowledgeModel.KnowledgeModelHandler
import Api.Handler.Migrator.MigratorHandler
import Api.Handler.Organization.OrganizationHandler
import Api.Handler.Package.PackageHandler
import Api.Handler.Questionnaire.QuestionnaireHandler
import Api.Handler.Token.TokenHandler
import Api.Handler.User.UserHandler
import Api.Handler.Version.VersionHandler
import Api.Middleware.AuthMiddleware
import Api.Middleware.CORSMiddleware
import Api.Middleware.JSONMiddleware
import Common.Context
import LensesConfig
import Model.Config.DSWConfig
import Model.Context.AppContext

unauthorizedEndpoints =
  [ (methodGet, mkRegex "^$")
  , (methodPost, mkRegex "^tokens$")
  , (methodGet, mkRegex "^export/.*$")
  , (methodPost, mkRegex "^users")
  , (methodPut, mkRegex "^users/.*/state")
  , (methodPut, mkRegex "^users/.*/password")
  , (methodPut, mkRegex "^users/.*/password?hash=.*")
  , (methodPost, mkRegex "^action-keys$")
  ]

loggingM :: Environment -> Middleware
loggingM Production = logStdout
loggingM Staging = logStdoutDev
loggingM Development = logStdoutDev
loggingM Test = id

createEndpoints :: AppContext -> ScottyT Text AppContextM ()
createEndpoints context
   --------------------
   -- MIDDLEWARES
   --------------------
 = do
  middleware (loggingM (context ^. config . environment . env))
  middleware corsMiddleware
  middleware jsonMiddleware
  middleware (authMiddleware (context ^. config) unauthorizedEndpoints)
   -- ------------------
   -- INFO
   -- ------------------
  get "/" getInfoA
   --------------------
   -- TOKENS
   --------------------
  post "/tokens" postTokenA
   --------------------
   -- ORGANIZATIONS
   --------------------
  get "/organizations/current" getOrganizationCurrentA
  put "/organizations/current" putOrganizationCurrentA
   --------------------
   -- USERS
   --------------------
  get "/users" getUsersA
  post "/users" postUsersA
  get "/users/current" getUserCurrentA
  get "/users/:userUuid" getUserA
  put "/users/current/password" putUserCurrentPasswordA
  put "/users/current" putUserCurrentA
  put "/users/:userUuid/password" putUserPasswordA
  put "/users/:userUuid" putUserA
  delete "/users/:userUuid" deleteUserA
  put "/users/:userUuid/state" changeUserStateA
  --  --------------------
  --  -- KNOWLEDGE MODEL
  --  --------------------
  get "/branches" getBranchesA
  post "/branches" postBranchesA
  get "/branches/:branchUuid" getBranchA
  put "/branches/:branchUuid" putBranchA
  delete "/branches/:branchUuid" deleteBranchA
  get "/branches/:branchUuid/km" getKnowledgeModelA
  get "/branches/:branchUuid/events" getEventsA
  post "/branches/:branchUuid/events/_bulk" postEventsA
  delete "/branches/:branchUuid/events" deleteEventsA
  put "/branches/:branchUuid/versions/:version" putVersionA
  get "/branches/:branchUuid/migrations/current" getMigrationsCurrentA
  post "/branches/:branchUuid/migrations/current" postMigrationsCurrentA
  delete "/branches/:branchUuid/migrations/current" deleteMigrationsCurrentA
  post "/branches/:branchUuid/migrations/current/conflict" postMigrationsCurrentConflictA
   --------------------
   -- PACKAGES
   --------------------
  get "/packages" getPackagesA
  get "/packages/unique" getUniquePackagesA
  get "/packages/:pkgId" getPackageA
  delete "/packages" deletePackagesA
  delete "/packages/:pkgId" deletePackageA
   --------------------
   -- ACTION KEYS
   --------------------
  post "/action-keys" postActionKeysA
   --------------------
   -- IMPORT/EXPORT
   --------------------
  post "/import" importA
  get "/export/:pkgId" exportA
   --------------------
   -- QUESTIONNAIRES
   --------------------
  get "/questionnaires" getQuestionnairesA
  post "/questionnaires" postQuestionnairesA
  get "/questionnaires/:qtnUuid" getQuestionnaireA
  put "/questionnaires/:qtnUuid/replies" putQuestionnaireRepliesA
  delete "/questionnaires/:qtnUuid" deleteQuestionnaireA
   --------------------
   -- ERROR
   --------------------
  notFound notFoundA

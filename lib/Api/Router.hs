module Api.Router where

import Network.HTTP.Types.Method (methodGet, methodPost, methodPut)
import Text.Regex
import Web.Scotty

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
import Api.Handler.Token.TokenHandler
import Api.Handler.User.UserHandler
import Api.Handler.Version.VersionHandler
import Api.Middleware.AuthMiddleware
import Api.Middleware.CORSMiddleware
import Common.Context
import Model.Config.DSWConfig

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

createEndpoints :: Context -> DSWConfig -> ScottyM ()
createEndpoints context dswConfig
   --------------------
   -- MIDDLEWARES
   --------------------
 = do
  middleware corsMiddleware
  middleware (authMiddleware dswConfig unauthorizedEndpoints)
   -- ------------------
   -- INFO
   -- ------------------
  get "/" (getInfoA context dswConfig)
   --------------------
   -- TOKENS
   --------------------
  post "/tokens" (postTokenA context dswConfig)
   --------------------
   -- ORGANIZATIONS
   --------------------
  get "/organizations/current" (getOrganizationCurrentA context dswConfig)
  put "/organizations/current" (putOrganizationCurrentA context dswConfig)
   --------------------
   -- USERS
   --------------------
  get "/users" (getUsersA context dswConfig)
  post "/users" (postUsersA context dswConfig)
  get "/users/current" (getUserCurrentA context dswConfig)
  get "/users/:userUuid" (getUserA context dswConfig)
  put "/users/current/password" (putUserCurrentPasswordA context dswConfig)
  put "/users/current" (putUserCurrentA context dswConfig)
  put "/users/:userUuid/password" (putUserPasswordA context dswConfig)
  put "/users/:userUuid" (putUserA context dswConfig)
  delete "/users/:userUuid" (deleteUserA context dswConfig)
  put "/users/:userUuid/state" (changeUserStateA context dswConfig)
   --------------------
   -- KNOWLEDGE MODEL
   --------------------
  get "/branches" (getBranchesA context dswConfig)
  post "/branches" (postBranchesA context dswConfig)
  get "/branches/:branchUuid" (getBranchA context dswConfig)
  put "/branches/:branchUuid" (putBranchA context dswConfig)
  delete "/branches/:branchUuid" (deleteBranchA context dswConfig)
  get "/branches/:branchUuid/km" (getKnowledgeModelA context dswConfig)
  get "/branches/:branchUuid/events" (getEventsA context dswConfig)
  post "/branches/:branchUuid/events/_bulk" (postEventsA context dswConfig)
  delete "/branches/:branchUuid/events" (deleteEventsA context dswConfig)
  put "/branches/:branchUuid/versions/:version" (putVersionA context dswConfig)
  get "/branches/:branchUuid/migrations/current" (getMigrationsCurrentA context dswConfig)
  post "/branches/:branchUuid/migrations/current" (postMigrationsCurrentA context dswConfig)
  delete "/branches/:branchUuid/migrations/current" (deleteMigrationsCurrentA context dswConfig)
  post "/branches/:branchUuid/migrations/current/conflict" (postMigrationsCurrentConflictA context dswConfig)
   --------------------
   -- PACKAGES
   --------------------
  get "/packages" (getPackagesA context dswConfig)
  get "/packages/unique" (getUniquePackagesA context dswConfig)
  get "/packages/:pkgId" (getPackageA context dswConfig)
  delete "/packages" (deletePackagesA context dswConfig)
  delete "/packages/:pkgId" (deletePackageA context dswConfig)
   --------------------
   -- ACTION KEYS
   --------------------
  post "/action-keys" (postActionKeysA context dswConfig)
   --------------------
   -- IMPORT/EXPORT
   --------------------
  post "/import" (importA context dswConfig)
  get "/export/:pkgId" (exportA context dswConfig)
   --------------------
   -- ERROR
   --------------------
  notFound notFoundA

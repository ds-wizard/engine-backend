module Api.Router where

import Text.Regex
import Web.Scotty

import Api.Handler.Common
import Api.Handler.Event.EventHandler
import Api.Handler.IO.IOHandler
import Api.Handler.Info.InfoHandler
import Api.Handler.KnowledgeModel.KnowledgeModelHandler
import Api.Handler.KnowledgeModelContainer.KnowledgeModelContainerHandler
import Api.Handler.Organization.OrganizationHandler
import Api.Handler.Package.PackageHandler
import Api.Handler.Token.TokenHandler
import Api.Handler.User.UserHandler
import Api.Handler.Version.VersionHandler
import Api.Middleware.Auth
import Api.Middleware.CORS
import Common.Context
import Common.DSPConfig

unauthorizedEndpoints =
  [mkRegex "^$", mkRegex "^tokens$", mkRegex "^export/.*$"]

createEndpoints :: Context -> DSPConfig -> ScottyM ()
createEndpoints context dspConfig
   --------------------
   -- MIDDLEWARES
   --------------------
 = do
  middleware corsMiddleware
  middleware (authMiddleware dspConfig unauthorizedEndpoints)
   --------------------
   -- INFO
   --------------------
  get "/" (getInfoA context dspConfig)
   --------------------
   -- TOKENS
   --------------------
  post "/tokens" (postTokenA context dspConfig)
   --------------------
   -- ORGANIZATIONS
   --------------------
  get "/organizations/current" (getOrganizationCurrentA context dspConfig)
  put "/organizations/current" (putOrganizationCurrentA context dspConfig)
   --------------------
   -- USERS
   --------------------
  get "/users" (getUsersA context dspConfig)
  post "/users/" (postUsersA context dspConfig)
  get "/users/current" (getUserCurrentA context dspConfig)
  get "/users/:userUuid" (getUserA context dspConfig)
  put "/users/current/password" (putUserCurrentPasswordA context dspConfig)
  put "/users/current" (putUserCurrentA context dspConfig)
  put "/users/:userUuid/password" (putUserPasswordA context dspConfig)
  put "/users/:userUuid" (putUserA context dspConfig)
  delete "/users/:userUuid" (deleteUserA context dspConfig)
   --------------------
   -- KNOWLEDGE MODEL
   --------------------
  get "/kmcs" (getKnowledgeModelContainersA context dspConfig)
  post "/kmcs" (postKnowledgeModelContainersA context dspConfig)
  get "/kmcs/:kmcUuid" (getKnowledgeModelContainerA context dspConfig)
  put "/kmcs/:kmcUuid" (putKnowledgeModelContainerA context dspConfig)
  delete "/kmcs/:kmcUuid" (deleteKnowledgeModelContainerA context dspConfig)
  get "/kmcs/:kmcUuid/km" (getKnowledgeModelA context dspConfig)
  get "/kmcs/:kmcUuid/events" (getEventsA context dspConfig)
  post "/kmcs/:kmcUuid/events/_bulk" (postEventsA context dspConfig)
  delete "/kmcs/:kmcUuid/events" (deleteEventsA context dspConfig)
  put "/kmcs/:kmcUuid/versions/:version" (putVersionA context dspConfig)
   --------------------
   -- PACKAGES
   --------------------
  get "/packages" (getPackagesA context dspConfig)
  get "/packages/unique" (getUniquePackagesA context dspConfig)
  get "/packages/:pkgId" (getPackageA context dspConfig)
  delete "/packages" (deletePackagesA context dspConfig)
  delete "/packages/:pkgId" (deletePackageA context dspConfig)
   --------------------
   -- IMPORT/EXPORT
   --------------------
  post "/import" (importA context dspConfig)
  get "/export/:pkgId" (exportA context dspConfig)
   --------------------
   -- ERROR
   --------------------
  notFound notFoundA

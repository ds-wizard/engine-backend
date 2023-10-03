module Wizard.Integration.Http.Admin.Runner where

import Control.Monad.Reader (asks)
import qualified Jose.Jwk as JWK

import Shared.Common.Integration.Http.Common.HttpClient
import Wizard.Integration.Http.Admin.RequestMapper
import Wizard.Integration.Http.Admin.ResponseMapper
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Service.App.AppHelper

retrieveJwtPublicKeys :: AppContextM JWK.JwkSet
retrieveJwtPublicKeys = do
  serverConfig <- asks serverConfig
  app <- getCurrentApp
  runRequest (toRetrieveJwtPublicKeysRequest app) toRetrieveJwtPublicKeysResponse

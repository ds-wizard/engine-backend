module Wizard.Database.DAO.OpenId.OpenIdClientSessionDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import Database.PostgreSQL.Simple
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.OpenId.OpenIdClientSession ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.OpenId.OpenIdClientSession

entityName = "openid_client_session"

findOpenIdClientSessionByState' :: String -> AppContextM (Maybe OpenIdClientSession)
findOpenIdClientSessionByState' state = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("state", state)]

insertOpenIdClientSession :: OpenIdClientSession -> AppContextM Int64
insertOpenIdClientSession = createInsertFn entityName

deleteOpenIdClientSessionByState :: String -> AppContextM Int64
deleteOpenIdClientSessionByState state = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("state", state)]

deleteExpiredOpenIdClientSessions :: AppContextM Int64
deleteExpiredOpenIdClientSessions = do
  let sql = fromString "DELETE FROM openid_client_session WHERE created_at < now() - interval '1 hour'"
  let action conn = execute_ conn sql
  runDB action

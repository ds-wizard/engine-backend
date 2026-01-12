module Wizard.Service.Plugin.PluginService where

import Control.Monad (void)
import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (id)

import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Plugin.PluginDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Plugin.PluginMapper

createPluginForAllTenants :: U.UUID -> String -> AppContextM ()
createPluginForAllTenants uuid url =
  runInTransaction $ do
    tenants <- findTenants
    now <- liftIO getCurrentTime
    traverse_
      ( \tenant -> do
          let plugin = toPlugin uuid url tenant.uuid now
          void $ insertPlugin plugin
      )
      tenants

createPluginForTenant :: U.UUID -> U.UUID -> String -> AppContextM ()
createPluginForTenant tenantUuid uuid url =
  runInTransaction $ do
    now <- liftIO getCurrentTime
    let plugin = toPlugin uuid url tenantUuid now
    void $ insertPlugin plugin

updatePluginsEnabled :: M.Map U.UUID Bool -> AppContextM ()
updatePluginsEnabled reqDto =
  runInTransaction $
    traverse_ (uncurry updatePluginEnabled) (M.toList reqDto)

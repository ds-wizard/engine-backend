module WizardLib.Public.Database.Migration.Development.OpenId.OpenIdClientMigration where

import Control.Monad (void)

import Shared.Common.Constant.Component
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import WizardLib.Public.Database.DAO.OpenId.OpenIdClientDefinitionDAO
import WizardLib.Public.Database.Migration.Development.OpenId.Data.OpenIdClients

runMigration :: AppContextC s sc m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(OpenId/OpenIdClient) started"
  _ <- deleteOpenIdClientDefinitionDefinitions
  void $ insertOpenIdClientDefinition defaultOpenIdClient
  logInfo _CMP_MIGRATION "(OpenId/OpenIdClient) ended"

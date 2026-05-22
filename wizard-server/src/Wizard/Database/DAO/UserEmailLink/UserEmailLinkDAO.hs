module Wizard.Database.DAO.UserEmailLink.UserEmailLinkDAO where

import Data.String
import Database.PostgreSQL.Simple
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext

deleteUserEmailLinksExpiredByTenantConfig :: AppContextM Int64
deleteUserEmailLinksExpiredByTenantConfig = do
  let sql =
        fromString
          "DELETE FROM user_email_link \
          \USING config_authentication \
          \WHERE user_email_link.tenant_uuid = config_authentication.tenant_uuid \
          \  AND user_email_link.created_at < (now() - (config_authentication.internal_user_email_link_expiration * interval '1 hour'));"
  logQuery sql ()
  let action conn = execute_ conn sql
  runDB action

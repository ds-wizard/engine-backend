module Wizard.Service.Domain.DomainService where

import Wizard.Database.DAO.App.AppDAO
import Wizard.Model.Context.AppContext

getDomain :: String -> AppContextM ()
getDomain appId = do
  _ <- findAppByAppId appId
  return ()

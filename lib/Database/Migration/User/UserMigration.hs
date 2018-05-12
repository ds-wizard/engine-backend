module Database.Migration.User.UserMigration where

import Control.Lens ((^.))
import Control.Monad.Logger (logInfo)
import Control.Monad.Reader (liftIO)

import Database.DAO.User.UserDAO
import Database.Migration.User.Data.Users
import LensesConfig

runMigration appContext = do
  $(logInfo) "MIGRATION (User/User): started"
  let context = appContext ^. oldContext
  let dswConfig = appContext ^. config
  liftIO $ deleteUsers context
  liftIO $ insertUser context userAlbert
  liftIO $ insertUser context userNikola
  liftIO $ insertUser context userIsaac
  $(logInfo) "MIGRATION (User/User): ended"

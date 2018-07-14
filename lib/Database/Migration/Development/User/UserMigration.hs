module Database.Migration.Development.User.UserMigration where

import Control.Monad.Logger (logInfo)

import Database.DAO.User.UserDAO
import Database.Migration.Development.User.Data.Users

runMigration = do
  $(logInfo) "MIGRATION (User/User): started"
  deleteUsers
  insertUser userAlbert
  insertUser userNikola
  insertUser userIsaac
  $(logInfo) "MIGRATION (User/User): ended"

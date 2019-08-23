module Database.Migration.Development.User.UserMigration where

import Constant.Component
import Database.DAO.User.UserDAO
import Database.Migration.Development.User.Data.Users
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(User/User) started"
  deleteUsers
  insertUser userAlbert
  insertUser userNikola
  insertUser userIsaac
  logInfo $ msg _CMP_MIGRATION "(User/User) ended"

module Wizard.Database.Migration.Development.User.UserMigration where

import Shared.Constant.Component
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(User/User) started"
  deleteUsers
  insertUser userAlbert
  insertUser userNikola
  insertUser userIsaac
  insertUser userCharles
  logInfo _CMP_MIGRATION "(User/User) ended"

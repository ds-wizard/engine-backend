module Wizard.Database.Migration.Development.User.UserMigration where

import Shared.Constant.Component
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.DAO.User.UserTokenDAO
import Wizard.Database.Migration.Development.User.Data.UserTokens
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(User/User) started"
  deleteUserTokens
  deleteUsers
  insertUser userSystem
  insertUser userAlbert
  insertUserToken albertToken
  insertUser userNikola
  insertUserToken nikolaToken
  insertUser userIsaac
  insertUserToken isaacToken
  insertUser userCharles
  logInfo _CMP_MIGRATION "(User/User) ended"

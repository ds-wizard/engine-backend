module Wizard.Database.Migration.Development.User.UserMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Migration.Development.User.Data.UserTokens
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Context.ContextLenses ()
import WizardLib.Public.Database.DAO.User.UserTokenDAO

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

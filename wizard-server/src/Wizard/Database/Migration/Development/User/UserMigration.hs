module Wizard.Database.Migration.Development.User.UserMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Migration.Development.User.Data.UserTokens
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Context.ContextLenses ()
import WizardLib.Public.Database.DAO.User.UserGroupDAO
import WizardLib.Public.Database.DAO.User.UserGroupMembershipDAO
import WizardLib.Public.Database.DAO.User.UserTokenDAO
import WizardLib.Public.Database.Migration.Development.User.Data.UserGroups

runMigration = do
  logInfo _CMP_MIGRATION "(User/User) started"
  deleteUserTokens
  deleteUserGroupMemberships
  deleteUsers
  deleteUserGroups
  insertUserGroup bioGroup
  insertUserGroup plantGroup
  insertUser userSystem
  insertUser userAlbert
  insertUserGroupMembership userAlbertBioGroupMembership
  insertUserToken albertToken
  insertUser userNikola
  insertUserToken nikolaToken
  insertUserGroupMembership userNikolaBioGroupMembership
  insertUser userIsaac
  insertUserToken isaacToken
  insertUser userCharles
  logInfo _CMP_MIGRATION "(User/User) ended"

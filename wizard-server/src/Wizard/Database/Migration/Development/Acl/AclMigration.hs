module Wizard.Database.Migration.Development.Acl.AclMigration where

import Shared.Common.Constant.Component
import Wizard.Database.DAO.Acl.GroupDAO
import Wizard.Database.Migration.Development.Acl.Data.Groups
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(ACL/ACL) started"
  deleteGroups
  insertGroup bioGroup
  insertGroup plantGroup
  logInfo _CMP_MIGRATION "(ACL/ACL) ended"

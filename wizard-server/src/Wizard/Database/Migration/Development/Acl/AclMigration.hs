module Wizard.Database.Migration.Development.Acl.AclMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Acl.GroupDAO
import Wizard.Database.Migration.Development.Acl.Data.Groups

runMigration = do
  logInfo _CMP_MIGRATION "(ACL/ACL) started"
  deleteGroups
  insertGroup bioGroup
  insertGroup plantGroup
  logInfo _CMP_MIGRATION "(ACL/ACL) ended"

module Registry.Database.Migration.Development.Migration
  ( runMigration
  ) where

import Registry.Constant.Component
import qualified Registry.Database.Migration.Development.Audit.AuditMigration as ADT
import qualified Registry.Database.Migration.Development.Organization.OrganizationMigration as ORG
import qualified Registry.Database.Migration.Development.Package.PackageMigration as PKG
import Registry.Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "started"
  ORG.runMigration
  PKG.runMigration
  ADT.runMigration
  logInfo $ msg _CMP_MIGRATION "ended"

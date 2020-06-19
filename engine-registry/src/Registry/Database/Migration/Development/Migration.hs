module Registry.Database.Migration.Development.Migration
  ( runMigration
  ) where

import Registry.Constant.Component
import qualified Registry.Database.Migration.Development.Audit.AuditMigration as ADT
import qualified Registry.Database.Migration.Development.Organization.OrganizationMigration as ORG
import qualified Registry.Database.Migration.Development.Package.PackageMigration as PKG
import qualified Registry.Database.Migration.Development.Template.TemplateMigration as TML
import Registry.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "started"
  ORG.runMigration
  PKG.runMigration
  ADT.runMigration
  TML.runMigration
  logInfo _CMP_MIGRATION "ended"

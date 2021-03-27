module Registry.Database.Migration.Development.Migration
  ( runMigration
  ) where

import qualified Registry.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ACK_Schema
import qualified Registry.Database.Migration.Development.Audit.AuditSchemaMigration as ADT_Schema
import qualified Registry.Database.Migration.Development.Organization.OrganizationMigration as ORG
import qualified Registry.Database.Migration.Development.Organization.OrganizationSchemaMigration as ORG_Schema
import qualified Registry.Database.Migration.Development.Package.PackageMigration as PKG
import qualified Registry.Database.Migration.Development.Package.PackageSchemaMigration as PKG_Schema
import qualified Registry.Database.Migration.Development.Template.TemplateMigration as TML
import qualified Registry.Database.Migration.Development.Template.TemplateSchemaMigration as TML_Schema
import Registry.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "started"
  -- 1. Build schema
  ORG_Schema.runMigration
  PKG_Schema.runMigration
  ACK_Schema.runMigration
  ADT_Schema.runMigration
  TML_Schema.runMigration
  -- 2. Load fixtures
  ORG.runMigration
  PKG.runMigration
  TML.runMigration
  logInfo _CMP_MIGRATION "ended"

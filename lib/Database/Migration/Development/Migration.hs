module Database.Migration.Development.Migration
  ( runMigration
  ) where

import Constant.Component
import qualified
       Database.Migration.Development.BookReference.BookReferenceMigration
       as BR
import qualified
       Database.Migration.Development.Branch.BranchMigration as B
import qualified
       Database.Migration.Development.Feedback.FeedbackMigration as F
import qualified
       Database.Migration.Development.Level.LevelMigration as LVL
import qualified
       Database.Migration.Development.Metric.MetricMigration as MTR
import qualified
       Database.Migration.Development.Migration.KnowledgeModel.MigratorMigration
       as KM_MIG
import qualified
       Database.Migration.Development.Migration.Questionnaire.MigratorMigration
       as QTN_MIG
import qualified
       Database.Migration.Development.Organization.OrganizationMigration
       as ORG
import qualified
       Database.Migration.Development.Package.PackageMigration as PKG
import qualified
       Database.Migration.Development.PublicPackage.PublicPackageMigration
       as PUBPKG
import qualified
       Database.Migration.Development.Questionnaire.QuestionnaireMigration
       as QTN
import qualified Database.Migration.Development.User.UserMigration
       as U
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "started"
  ORG.runMigration
  U.runMigration
  PKG.runMigration
  B.runMigration
  KM_MIG.runMigration
  QTN_MIG.runMigration
  QTN.runMigration
  PUBPKG.runMigration
  BR.runMigration
  F.runMigration
  MTR.runMigration
  LVL.runMigration
  logInfo $ msg _CMP_MIGRATION "ended"

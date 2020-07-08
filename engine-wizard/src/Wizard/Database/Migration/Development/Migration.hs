module Wizard.Database.Migration.Development.Migration
  ( runMigration
  ) where

import Shared.Constant.Component
import qualified Wizard.Database.Migration.Development.BookReference.BookReferenceMigration as BR
import qualified Wizard.Database.Migration.Development.Branch.BranchMigration as B
import qualified Wizard.Database.Migration.Development.Config.AppConfigMigration as CFG
import qualified Wizard.Database.Migration.Development.Document.DocumentMigration as DOC
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackMigration as F
import qualified Wizard.Database.Migration.Development.Level.LevelMigration as LVL
import qualified Wizard.Database.Migration.Development.Metric.MetricMigration as MTR
import qualified Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorMigration as KM_MIG
import qualified Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorMigration as QTN_MIG
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "started"
  CFG.runMigration
  U.runMigration
  PKG.runMigration
  B.runMigration
  KM_MIG.runMigration
  QTN_MIG.runMigration
  QTN.runMigration
  BR.runMigration
  F.runMigration
  MTR.runMigration
  LVL.runMigration
  DOC.runMigration
  TML.runMigration
  logInfo _CMP_MIGRATION "ended"

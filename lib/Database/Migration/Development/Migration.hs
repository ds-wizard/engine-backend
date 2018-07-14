module Database.Migration.Development.Migration where

import Control.Monad.Logger (logInfo)

import qualified
       Database.Migration.Development.BookReference.BookReferenceMigration
       as BR
import qualified
       Database.Migration.Development.Branch.BranchMigration as B
import qualified
       Database.Migration.Development.Feedback.FeedbackMigration as F
import qualified
       Database.Migration.Development.Metric.MetricMigration as MTR
import qualified
       Database.Migration.Development.Migrator.MigratorMigration as MIG
import qualified
       Database.Migration.Development.Organization.OrganizationMigration
       as ORG
import qualified
       Database.Migration.Development.Package.PackageMigration as PKG
import qualified
       Database.Migration.Development.PublicQuestionnaire.PublicQuestionnaireMigration
       as PUBQTN
import qualified
       Database.Migration.Development.Questionnaire.QuestionnaireMigration
       as QTN
import qualified Database.Migration.Development.User.UserMigration
       as U

runMigration = do
  $(logInfo) "MIGRATION: started"
  ORG.runMigration
  U.runMigration
  PKG.runMigration
  B.runMigration
  MIG.runMigration
  QTN.runMigration
  PUBQTN.runMigration
  BR.runMigration
  F.runMigration
  MTR.runMigration
  $(logInfo) "MIGRATION: ended"
